-module(billy_g2a_handler).

-export([init/2]).
-export([get_payment_link/1]).


init(Req, OptsMap) ->
    
    #{method := QueryMethod} = Req,
    #{method := ApiMethod} = OptsMap,

    ApiRes = case ApiMethod of
		 redirector -> redirector(Req);
		 notification -> notification(Req);
		 success -> success(Req);
		 fail -> fail(Req);
		 _ -> unknown_method()
	     end,
    
    Resp = case ApiRes of
	       {output, Body} ->
		   Headers = #{<<"content-type">> => <<"text/plain">>},
		   cowboy_req:reply(200, Headers, Body, Req);
	       {output, Body, Headers} ->
		   cowboy_req:reply(200, Headers, Body, Req);
	       {redirect, Url} ->
		   cowboy_req:reply(303, #{<<"location">> => Url}, Req)
	   end,
    
    {ok, Resp, OptsMap}.



redirector(Req) ->
    io:format("DEBUG>>> billy_g2a_handler:redirector Req:~p~n" , [Req]),

    %% Получить параметры GET запроса 
    #{tr_id := TrIdBin, signature := SignatureBin} = cowboy_req:match_qs([tr_id, signature], Req),

    io:format("DEBUG>>> billy_g2a_handler:redirector TrIdBin:~p~n" , [TrIdBin]),
    io:format("DEBUG>>> billy_g2a_handler:redirector SignatureBin:~p~n" , [SignatureBin]),

    GetData = [
	       {tr_id, TrIdBin},
	       {signature, SignatureBin}
	      ],

    %% Проверить полученные данные
    case check_get_data(redirector, GetData) of
	{error, CheckRes} ->
	    {output, <<"ERROR">>};
	{ok, NormGetData} ->
	    %% Получить транзакцию
	    TrId = proplists:get_value(tr_id, NormGetData),
	    case billy_cbserver:get_transaction(#{transaction_id => TrId, res_type => json}) of
	    	{ok, TrProplist} ->
	    	    %% io:format("DEBUG>>> billy_bpay_handler:redirector TrProplist:~p~n" , [TrProplist]),

	    	    %% Проверить статус транзакции
	    	    TrStatus = proplists:get_value(<<"status">>, TrProplist),
		    case TrStatus of
			0 ->
			    %% Получить мерчанта		    
			    MerchantId = proplists:get_value(<<"user_id">>, TrProplist),
			    case billy_cbserver:get_user(#{user_id => MerchantId, res_type => json}) of
				{ok, [MerchantUserProplist]} ->

				    MerchantName = proplists:get_value(<<"name">>, MerchantUserProplist),

				    %% Получаем параметры
				    MerchantParamsStr = proplists:get_value(<<"params">>, MerchantUserProplist),
				    MerchantParams = jiffy:decode(MerchantParamsStr, [return_maps]),

				    %% Получаем секретный ключ мерчанта
				    MerchantSecretKey = maps:get(<<"secret_key">>, MerchantParams),

				    %% Формируем сигнатуру
				    TrParamsBinJson = proplists:get_value(<<"params">>, TrProplist),
				    TrParams = jiffy:decode(TrParamsBinJson, [return_maps]),

				    %% io:format("DEBUG>>> billy_bpay_handler:redirector TrParams=~p~n", [TrParams]),

				    BillId = maps:get(<<"bill_id">>, TrParams),
				    Amount = proplists:get_value(<<"cost">>, TrProplist),
				    Ccy = proplists:get_value(<<"currency_alpha">>, TrProplist),

				    BillySignParams = #{bill_id => BillId, amount => Amount, ccy => Ccy, merchant_secret_key => MerchantSecretKey},
				    BillySign = billy_commons:get_billy_signature(BillySignParams),

				    %% Проверить сигнатуру
				    QuerySign = proplists:get_value(signature, NormGetData),

				    %% io:format("DEBUG>>> billy_bpay_handler:redirector QuerySign:~p  BillySign:~p~n" , [QuerySign, BillySign]),

				    case BillySign of
					%% Сигнатура в порядке
					QuerySign ->
					    io:format("DEBUG>>> billy_g2a_handler:redirector QuerySign:~p  BillySign:~p~n" , [QuerySign, BillySign]),

					    %% Получить токен платежа из параметров транзакции
					    G2AToken = maps:get(<<"token">>, TrParams),

					    %% Сформировать ссылку перехода на оплату

					    L = list_to_binary(io_lib:format("https://checkout.pay.g2a.com/index/gateway?token=~ts", [G2AToken])),
					    
					    {ok, G2ARedirectHtml} = billy_g2a_dtl:render([ {g2a_payment_link, L} ]),
					    
					    %% Вернуть результат
					    Body = G2ARedirectHtml,
					    {output, Body, #{<<"content-type">> => <<"text/html">>}};
					_ -> 
					    Body = <<"signature check error">>,
					    {output, Body}
				    end;
				_ ->
				    Body = <<"merchant not found">>,
				    {output, Body}
			    end;
			_ ->
			    Body = <<"transaction status error">>,
			    {output, Body}
		    end;
		_ ->
		    Body = <<"transaction not found">>,
		    {output, Body}
	    end
    end.



notification(Req) ->
    io:format("DEBUG>>> billy_g2a_handler:notification! Req0=~p~n", [Req]),

    %% type=payment
    %% &transactionId=eac61839-7db6-4cab-8ec3-9708c4676938
    %% &userOrderId=70001010467320
    %% &amount=100.0
    %% &currency=EUR
    %% &status=complete
    %% &orderCreatedAt=2015-02-20 01:21:35
    %% &orderCompleteAt=2015-02-20 01:25:51
    %% &refundedAmount=0
    %% &provisionAmount=0
    %% &hash=2a24c939992bc5b2e09480a7cb7acbf2cda32278ecca912457678008ff3a1fdf

    %% Прочитать параметры и авторизовать платёж
    {ok, ReqParamsKV, _} = cowboy_req:read_urlencoded_body(Req),
    io:format("DEBUG>>> billy_g2a_handler:notification  ReqParamsKV=~p~n", [ReqParamsKV]),

    %% Проверить параметры на валидность
    PostData = [
		{type, proplists:get_value(<<"type">>, ReqParamsKV)},
		{transaction_id, proplists:get_value(<<"transactionId">>, ReqParamsKV)},
		{user_order_id, proplists:get_value(<<"userOrderId">>, ReqParamsKV)},
		{amount, proplists:get_value(<<"amount">>, ReqParamsKV)},
		{currency, proplists:get_value(<<"currency">>, ReqParamsKV)},
		{status, proplists:get_value(<<"status">>, ReqParamsKV)}
	       ],

    %% Проверить параметры на валидность
    case check_post_data(notification, PostData) of
	{error, CheckRes} ->
	    Responce = {[ {error, {[ {message, "Query params error!"} ]}} ]},
	    {output, jiffy:encode(Responce)};
	{ok, NormPostData} ->
	    %% Проверить тип и статус оповещения
	    Type = proplists:get_value(type, NormPostData),
	    Status = proplists:get_value(status, NormPostData),

	    case {Type, Status} of
		{"payment", 
		 "complete"} ->
		    %% Проверить сигнатуру
		    G2ATransactionIdStr = proplists:get_value(transaction_id, NormPostData),
		    BillId = proplists:get_value(user_order_id, NormPostData),
		    AmountStr = proplists:get_value(<<"amount">>, ReqParamsKV),
		    G2AHash = binary_to_list(proplists:get_value(<<"hash">>, ReqParamsKV)),
		    case check_g2a_signature(G2ATransactionIdStr, BillId, AmountStr, G2AHash) of
			true ->
			    %% Получить транзакцию связанную с оплатой
			    case billy_cbserver:get_transaction(#{transaction_id => BillId, res_type => json}) of
				{ok, TrProplist} ->
				    G2A_Amount = erlang:round(proplists:get_value(amount, NormPostData) * 100),
				    G2A_Currency = list_to_binary(proplists:get_value(currency, NormPostData)),
				    
				    TrCost = proplists:get_value(<<"cost">>, TrProplist),
				    TrCurrency = proplists:get_value(<<"currency_alpha">>, TrProplist),
				    
				    %% Проверить сумму транзакции
				    case {G2A_Amount, G2A_Currency} of
					{TrCost, TrCurrency}  ->
					    ProcessResult = <<"success">>,
					    
					    case billy_payment:process_transaction(#{transaction_id => BillId, process_result => ProcessResult}) of
						{ok, _NewBalance} ->

						    %% Немедленно вызвать IPN к мерчанту
						    NotifyParamsMap = #{transaction_id => BillId},
						    wpool:cast(billy_ipn_wpool, {notify, NotifyParamsMap}),
						    
						    %% Вернуть результат
						    Responce = {[ {result, {[ {message, "All right!"} ]}} ]},
						    {output, jiffy:encode(Responce)};
						{error, transaction_already_processed} ->
						    Responce = {[ {result, {[ {message, "All right!"} ]}} ]},
						    {output, jiffy:encode(Responce)};
						{error, Reason} ->
						    Responce = {[ {error,  {[ {message, "Payment process error!"} ]}} ]},
						    {output, jiffy:encode(Responce)}
					    end;
					{TrCost, _} -> 
					    Responce = {[ {error,  {[ {message, "Transaction currency error!"} ]}} ]},
					    {output, jiffy:encode(Responce)};
					{_, TrCurrency} -> 
					    Responce = {[ {error,  {[ {message, "Transaction cost error!"} ]}} ]},
					    {output, jiffy:encode(Responce)};
					_ ->
					    Responce = {[ {error,  {[ {message, "System error!"} ]}} ]},
					    {output, jiffy:encode(Responce)}
				    end
			    end;
			_ ->
			    Responce = {[ {error, {[ {message, "Query signature error!"} ]}} ]},
			    {output, jiffy:encode(Responce)}
		    end;
		_ ->
		    Responce = {[ {error, {[ {message, "Query type or status error!"} ]}} ]},
		    {output, jiffy:encode(Responce)}
	    end	
    end.




success(Req) ->
    %% Прочитать параметр order
    %% #{order := OrderId} = cowboy_req:match_qs([order], Req),

    Qs = cowboy_req:parse_qs(Req),
    io:format("DEBUG>>> billy_qiwi_handler:success! Qs=~p~n", [Qs]),

    %% TODO :
    %% Получить данные по транзакции
    %% Определить, задавались ли URL перенаправления в запросе
    %% Если нет - 
    %% Получить данные по мерчанту транзакции
    %% Получить ссылки перенаправления из настроек мерчанта
    
    {redirect, <<"https://gocs.pro">>}.


fail(Req) ->
    %% io:format("DEBUG>>> billy_qiwi_handler:fail! Req=~p~n", [Req]),
    %% Прочитать параметр order
    {redirect, <<"https://gocs.pro">>}.


%% Функция генерирует ссылку для оплаты заказа в системе g2a
get_payment_link(#{tr_id := TrId, tr_cost := TrCost, tr_ccy := TrCurrency, signature := Signature}) ->
    %% Вызываем API полатёжного шлюза G2A и получаем token платежа
    Query = "https://checkout.pay.g2a.com/index/createQuote",
    
    MerchantEmail = billy_config:get(g2apay_merchant_email),
    AuthorizationStr = io_lib:format("~ts~ts~ts", [billy_config:get(g2apay_api_key), MerchantEmail, billy_config:get(g2apay_secret)]),
    AuthorizationHash = crypto:hash(sha256, AuthorizationStr),
    AuthorizationHashVal = lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= AuthorizationHash]),
    AuthorizationHeaderData = io_lib:format("~ts; ~ts", [billy_config:get(g2apay_api_key), AuthorizationHashVal]),
    
    FormatedTrCost = float_to_list(TrCost / 100, [{decimals, 2}]),
    
    Items = io_lib:format("[{\"sku\":\"1\",\"name\":\"Balance Refill\",\"amount\":\"~ts\",\"qty\":\"1\",\"price\":\"~ts\",\"id\":\"~p\",\"url\":\"http://somacase.com\"}]",
			  [FormatedTrCost, FormatedTrCost, TrId]),

    QueryHashStr = io_lib:format("~p~ts~ts~ts", [TrId, FormatedTrCost, TrCurrency, billy_config:get(g2apay_secret)]),
    QueryHash = crypto:hash(sha256, QueryHashStr),
    QueryHashVal = lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= QueryHash]),
    
    %% Body = io_lib:format("api_hash=~ts&hash=~ts&order_id=~p&amount=~ts&currency=~ts&email=~ts&url_failure=~ts&url_ok=~ts&items=~ts",
    %% 			 [config:get(g2apay_api_key), QueryHashVal, TrId, FormatedTrCost, TrCurrency, MerchantEmail, "https://gocs.pro", "https://gocs.pro", Items]),

    Body = io_lib:format("api_hash=~ts&hash=~ts&order_id=~p&amount=~ts&currency=~ts&url_failure=~ts&url_ok=~ts&items=~ts",
     			 [billy_config:get(g2apay_api_key), 
			  QueryHashVal, 
			  TrId, 
			  FormatedTrCost, 
			  TrCurrency, 
			  "https://somacase.com/payment/g2a/fail", 
			  "https://somacase.com/payment/g2a/success", 
			  Items]),
        

    io:format("DEBUG>>> billy_g2a_handler:get_payment_link ibrowse POST Body: ~ts~n", [Body]),
    
    %% Вызываем API полатёжного шлюза qiwi и получаем параметры выставленного счёта
    Headers = [
	       {<<"Authorization">>, list_to_binary(AuthorizationHeaderData)},
	       {<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}
	      ],
    Options = [],
    G2AGateRes = case hackney:request(post, Query, Headers, Body, Options) of
		     %% Запрос выполнен успешно
		     {ok, 200, RespHeaders, ClientRef}=HR ->
			 %% Получить тело ответа и определить что сервер мерчанта закрыл транзакцию
			 {ok, RespBody} = hackney:body(ClientRef),
			 
			 io:format("DEBUG>>> billy_g2a_handler:get_payment_link HackeyReq:=~p~n", [HR]),
			 
			 DecodedRespBody = jiffy:decode(RespBody, [return_maps]),
			 
			 io:format("DEBUG>>> billy_g2a_handler:get_payment_link Query DecodedRespBody = ~p~n", [DecodedRespBody]),
			 
			 case maps:get(<<"status">>, DecodedRespBody, <<"error">>) of
			     <<"ok">> ->
				 G2AToken = maps:get(<<"token">>, DecodedRespBody),

				 case billy_cbserver:get_transaction(#{transaction_id => TrId, res_type => json}) of
				     {ok, TrProplist} ->
					 
					 %% Сохранить токен в параметрах транзакции
					 TrParamsBin = proplists:get_value(<<"params">>, TrProplist),
					 TrParams = jiffy:decode(TrParamsBin, [return_maps]),
					 NewTrParams = maps:merge(TrParams, #{token => G2AToken}),
					 NewTrParamsBin = jiffy:encode(NewTrParams),
					 
					 error_logger:info_msg("DEBUG>>>billy_g2a_handler:get_payment_link #~p  NewTrParamsBin: ~ts~n", [TrId, NewTrParamsBin]),

					 billy_cbserver:update_transaction(#{transaction_id => TrId, params => NewTrParamsBin, params_type => json_str}),
					 
					 %% Перенаправить пользователя на редиректор платёжного шлюза
					 BaseLink = "https://somacase.com/payment/g2a/redirector",
					 %% BaseLink = "http://127.0.0.1:8008/payment/bpay/redirector",

					 L = unicode:characters_to_binary(io_lib:format("~ts?tr_id=~p&signature=~ts", [BaseLink, TrId, Signature])),
					 io:format("DEBUG>>> billy_g2a_handler:get_payment_link  L>>>   ~ts~n", [L]),

					 {ok, L}
				 end
			 end
    end,
    case G2AGateRes of
	{ok, G2AGateLink} -> G2AGateLink;
	{error, g2apay_gate_down} -> <<"/">>
    end.




unknown_method() ->
    io:format("DEBUG>>> billy_api_handler:unknown_method!~n"),
    Body = <<"Unknown method">>,
    {ok, Body}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Функции проверки данных POST запросов

check_get_data(redirector, GetData) ->
    TrId = proplists:get_value(tr_id, GetData),
    Signature = proplists:get_value(signature, GetData),
    CheckResult = [
		   {tr_id, billy_query_helper:check_integer(TrId)},
		   {signature, billy_query_helper:check_string(Signature, 64)}
		  ],
    FinCheck = billy_query_helper:finaly_check(CheckResult),
    
    case FinCheck of
	true  -> {ok, CheckResult};
	false -> {error, CheckResult}
    end.

check_post_data(notification, PostData) ->
    TypeStr = proplists:get_value(type, PostData),
    TransactionIdStr = proplists:get_value(transaction_id, PostData),
    UserOrderIdStr = proplists:get_value(user_order_id, PostData),
    AmountStr = proplists:get_value(amount, PostData),
    CurrencyStr = proplists:get_value(currency, PostData),
    StatusStr = proplists:get_value(status, PostData),

    CheckResult = [
    		   {type, billy_query_helper:check_string(TypeStr, 20)},
    		   {transaction_id, billy_query_helper:check_string(TransactionIdStr, 128)},
    		   {user_order_id, billy_query_helper:check_integer(UserOrderIdStr)},
    		   {amount, billy_query_helper:check_float(AmountStr)},
    		   {currency, billy_query_helper:check_string(CurrencyStr, 5)},
    		   {status, billy_query_helper:check_string(StatusStr, 20)}
    		  ],
    FinCheck = billy_query_helper:finaly_check(CheckResult),
    
    case FinCheck of
    	true  -> {ok, CheckResult};
    	false -> {error, CheckResult}
    end.

check_g2a_signature(TransactionIdStr, UserOrderId, AmountStr, G2AHash) ->
    io:format("DEBUG>>> billy_g2a_handler:check_g2a_signature G2AHash: ~ts~n", [G2AHash]),
    case calc_g2a_signature(TransactionIdStr, UserOrderId, AmountStr) of
	G2AHash -> true;
	_ -> false
    end.

calc_g2a_signature(TransactionIdStr, UserOrderId, AmountStr) ->
    HashStr = io_lib:format("~ts~p~ts~ts", [TransactionIdStr, UserOrderId, AmountStr, billy_config:get(g2apay_secret)]),
    Hash = crypto:hash(sha256, HashStr),
    HashVal = lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= Hash]),
    io:format("DEBUG>>> billy_g2a_handler:calc_g2a_signature HashVal: ~ts~n", [HashVal]),
    HashVal.



