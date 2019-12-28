-module(billy_g2a_handler).

-export([init/2]).

init(Req, OptsMap) ->
    
    #{method := QueryMethod} = Req,
    #{method := ApiMethod} = OptsMap,

    ApiRes = case ApiMethod of
		 notification -> notification(Req);
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
		{"payment", "complete"} ->

		    BillyTrId = proplists:get_value(user_order_id, NormPostData),

		    %% Получить транзакцию связанную с оплатой
		    {ok, [TrProplist | _]} = billy_transaction:get(#{transaction_id => BillyTrId}),
		    TrParamsStr = proplists:get_value(<<"params">>, TrProplist),
		    TrParamsMap = jiffy:decode(TrParamsStr, [return_maps]),
		    
		    %% Загружаем конфиг мерчанта по платёжной системе
		    MerchantId = proplists:get_value(<<"merchant_id">>, TrProplist),
		    PaySystemKey = maps:get(<<"system">>, TrParamsMap),
		    {ok, PaysystemConfigMap} = billy_config:get(#{merchant_id => MerchantId, paysystem_key => PaySystemKey}), 
		    G2APaySecret = maps:get(<<"g2apay_secret">>, PaysystemConfigMap),

		    %% Проверить сигнатуру
		    G2ATransactionIdStr = proplists:get_value(transaction_id, NormPostData),
		    AmountStr = proplists:get_value(<<"amount">>, ReqParamsKV),
		    ReqSignature = binary_to_list(proplists:get_value(<<"hash">>, ReqParamsKV)),
		    case check_g2a_signature(G2ATransactionIdStr, BillyTrId, AmountStr, ReqSignature, G2APaySecret) of
			true ->
			    G2A_Amount = erlang:round(proplists:get_value(amount, NormPostData) * 100),
			    G2A_Currency = list_to_binary(proplists:get_value(currency, NormPostData)),

			    TrAmount = proplists:get_value(<<"amount">>, TrProplist),
			    TrCurrency = proplists:get_value(<<"currency_alpha">>, TrProplist),

			    %% Проверить сумму транзакции
			    case {G2A_Amount, G2A_Currency} of
				{TrAmount, TrCurrency}  ->
				    ProcessResult = <<"success">>,

				    case billy_payment:process_transaction(#{transaction_id => BillyTrId, process_result => ProcessResult}) of
					{ok, _NewBalance} ->
					    Responce = {[ {result, {[ {message, "All right!"} ]}} ]},
					    {output, jiffy:encode(Responce)};

					{error, transaction_already_processed} ->
					    Responce = {[ {result, {[ {message, "All right!"} ]}} ]},
					    {output, jiffy:encode(Responce)};

					{error, Reason} ->
					    Responce = {[ {error,  {[ {message, "Payment process error!"} ]}} ]},
					    {output, jiffy:encode(Responce)}
				    end;
				{TrAmount, _} -> 
				    Responce = {[ {error,  {[ {message, "Transaction currency error!"} ]}} ]},
				    {output, jiffy:encode(Responce)};
				{_, TrCurrency} -> 
				    Responce = {[ {error,  {[ {message, "Transaction cost error!"} ]}} ]},
				    {output, jiffy:encode(Responce)};
				_ ->
				    Responce = {[ {error,  {[ {message, "System error!"} ]}} ]},
				    {output, jiffy:encode(Responce)}
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



unknown_method() ->
    io:format("DEBUG>>> billy_api_handler:unknown_method!~n"),
    Body = <<"Unknown method">>,
    {ok, Body}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Функции проверки данных POST запросов

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



check_g2a_signature(G2ATransactionIdStr, UserOrderId, AmountStr, ReqSignature, G2APaySecret) ->
    calc_g2a_signature(G2ATransactionIdStr, UserOrderId, AmountStr, G2APaySecret) == ReqSignature.

calc_g2a_signature(G2ATransactionIdStr, UserOrderId, AmountStr, G2APaySecret) ->
    HashStr = io_lib:format("~ts~p~ts~ts", [G2ATransactionIdStr, UserOrderId, AmountStr, G2APaySecret]),
    Hash = crypto:hash(sha256, HashStr),
    HashVal = lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= Hash]),
    HashVal.



