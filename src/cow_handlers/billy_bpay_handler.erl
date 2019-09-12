-module(billy_bpay_handler).

-export([init/2]).
-export([get_payment_link/1]).

-include_lib("xmerl/include/xmerl.hrl"). 

init(Req, OptsMap) ->
    
    #{method := QueryMethod} = Req,
    #{method := ApiMethod} = OptsMap,

    ApiRes = case ApiMethod of
		 notification -> notification(Req);
		 success -> success(Req);
		 fail -> fail(Req);
		 redirector -> redirector(Req);
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
    io:format("DEBUG>>> billy_bpay_handler:notification  Req=~p~n", [Req]),

    %% Прочитать параметры и авторизовать платёж
    {ok, ReqParamsKV, _} = cowboy_req:read_urlencoded_body(Req),
    io:format("DEBUG>>> billy_bpay_handler:notification  ReqParamsKV=~p~n", [ReqParamsKV]),

    PostData = [
		{data, proplists:get_value(<<"data">>, ReqParamsKV)},
		{key, proplists:get_value(<<"key">>, ReqParamsKV)}
	       ],

    case check_post_data(notification, PostData) of
    	{error, CheckRes} ->
    	    {output, <<"ERROR">>};
    	{ok, NormPostData} ->

	    %% Раcкодировать параметры
	    BPayFormDataBase64 = proplists:get_value(data, NormPostData),
	    BPayFormDataXmlStr0 = binary_to_list(base64:decode(BPayFormDataBase64)),

	    io:format("DEBUG>>> billy_bpay_handler:notification  BPayFormDataXmlStr0=~p~n", [BPayFormDataXmlStr0]),

	    BPayFormDataXmlStr = re:replace(BPayFormDataXmlStr0, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),

	    io:format("DEBUG>>> billy_bpay_handler:notification  BPayFormDataXmlStr=~p~n", [BPayFormDataXmlStr]),

	    %% Проверить сигнатуру
	    BPaySignature = billy_config:get(bpay_signature),
	    InBPayFormKey = proplists:get_value(key, NormPostData),
	    case check_bpay_formkey(#{bpay_form_data => BPayFormDataXmlStr0,
				      bpay_signature => BPaySignature,
				      in_bpay_form_key => InBPayFormKey}) of
		true ->
    		    %% получить транзакцию связанную с оплатой
		    {BPayFormData, _} = xmerl_scan:string(BPayFormDataXmlStr),
		    TrIdStr = xml_val(xmerl_xpath:string("order_id", BPayFormData)),
    		    TrId = billy_query_helper:check_integer(TrIdStr),
    		    case billy_cbserver:get_transaction(#{transaction_id => TrId, res_type => json}) of
    			{ok, TrProplist} ->

			    io:format("DEBUG>>> billy_bpay_handler:notification  TrProplist=~p~n", [TrProplist]),

    			    %% Проверить сумму и валюту платежа
			    BPayCostStr = xml_val(xmerl_xpath:string("amount", BPayFormData)),
			    BPayCostFloat = billy_query_helper:check_float(BPayCostStr),
    			    BPayCost = erlang:round(BPayCostFloat * 100),
    			    BPayCurrency = <<"USD">>,

    			    TrCost = proplists:get_value(<<"cost">>, TrProplist),
    			    TrCurrency = proplists:get_value(<<"currency_alpha">>, TrProplist),

    			    %% Проверить сумму транзакции
    			    case {BPayCost, BPayCurrency} of
    				{TrCost, TrCurrency} ->
    				    %% Обработать транзакцию. т.е.
    				    %% 1) Обновить статус по базе что транзакция принята
    				    %% 2) Обновить баланс мерчанта
    				    %% TrType = proplists:get_value(<<"type">>, TrProplist),
				    
    				    ProcessResult = <<"success">>,
				    
    				    case billy_payment:process_transaction(#{transaction_id => TrId, process_result => ProcessResult}) of
    					{ok, _NewBalance} ->
					    
    					    %% Немедленно вызвать IPN к мерчанту
    					    NotifyParamsMap = #{transaction_id => TrId},
    					    wpool:cast(billy_ipn_wpool, {notify, NotifyParamsMap}),

    					    %% Вернуть результат
					    BPayResponseXml = {result,  
							       [
								{code, ["100"]},
								{text, ["success"]}
							       ]
							      },
					    BPayResponse = billy_commons:make_doc_xml(BPayResponseXml, "<?xml version='1.0' encoding=\"utf8\"?>"),
    					    {output, BPayResponse};
    					{error, transaction_already_processed} ->
    					    %% Вернуть результат
					    BPayResponseXml = {result,  
							       [
								{code, ["100"]},
								{text, ["success"]}
							       ]
							      },
					    BPayResponse = billy_commons:make_doc_xml(BPayResponseXml, "<?xml version='1.0' encoding=\"utf8\"?>"),
    					    {output, BPayResponse};
    					{error, Reason} ->
    					    {output, <<"SYSTEM PROCESS TRANSACTION ERROR">>}
    				    end;
    				{TrCost, _} -> 
    				    {output, <<"CURRENCY ERROR">>};
    				{_, TrCurrency} -> 
    				    {output, <<"AMOUNT ERROR">>};
    				_ ->
    				    {output, <<"SYSTEM ERROR">>}
    			    end
    		    end;
		false ->
		    {output, <<"SIGNATURE ERROR">>}
	    end
    end.


redirector(Req) ->
    io:format("DEBUG>>> billy_bpay_handler:redirector Req:~p~n" , [Req]),

    %% Получить параметры GET запроса 
    #{tr_id := TrIdBin, signature := SignatureBin} = cowboy_req:match_qs([tr_id, signature], Req),

    io:format("DEBUG>>> billy_bpay_handler:redirector TrIdBin:~p~n" , [TrIdBin]),
    io:format("DEBUG>>> billy_bpay_handler:redirector SignatureBin:~p~n" , [SignatureBin]),

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

				    %% Получаем антифрод параметры, и кодируем их в json строку
				    AntiFraudParams = maps:get(<<"anti_fraud_params">>, TrParams),				    
				    AntiFraudParamsJson = binary_to_list(jiffy:encode(AntiFraudParams)),

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
					    io:format("DEBUG>>> billy_bpay_handler:redirector QuerySign:~p  BillySign:~p~n" , [QuerySign, BillySign]),

					    BPayMerchantId = billy_config:get(bpay_merchantid),

					    AmountStr = float_to_list(Amount / 100, [{decimals, 2}]),

					    %% Сформировать платёжную форму bpay
					    Comment = io_lib:format("Order #~p (Game ~ts)", [TrId, MerchantName]),

					    SuccessUrl = "https://bvvd.ru/payment/bpay/success",
					    FailUrl = "https://bvvd.ru/payment/bpay/fail",
					    CallbackUrl = "https://bvvd.ru/payment/bpay/notification",
					    BpayFormDataXml = {payment,  
							       [
								{type, ["1.2"]},
								{merchantid, [BPayMerchantId]},
								{amount, [AmountStr]},
								{description, [Comment]},
								{method, ["card_usd"]},
								{order_id, [integer_to_list(TrId)]},
								{success_url, [SuccessUrl]},
								{fail_url, [FailUrl]},
								{callback_url, [CallbackUrl]},
								{advanced1, [AntiFraudParamsJson]},
								{lang, ["en"]},
								{istest, ["0"]}
							       ]
							      },
					    
					    BPayFormData = billy_commons:make_doc_xml(BpayFormDataXml, ""),
					    BPayFormDataBase64 = base64:encode(BPayFormData),

					    io:format("DEBUG>>> billy_bpay_handler:redirector BPayFormData:~p~n" , [BPayFormData]),

					    BPaySignature = billy_config:get(bpay_signature),
					    BPayFormKeyA = lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= erlang:md5(BPayFormData)]),
					    BPayFormKeyB = lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= erlang:md5(BPaySignature)]),

					    io:format("DEBUG>>> billy_bpay_handler:redirector BPayFormKeyA:~p~n" , [BPayFormKeyA]),

					    %% BPayFormKey0 = erlang:md5([BPayFormKeyA | BPayFormKeyB]),
					    BPayFormKey0 = erlang:md5(
					     		     lists:flatten(
					     		       io_lib:format("~ts~ts", [BPayFormKeyA, BPayFormKeyB])
							      )
					     		    ),
					    BPayFormKey = lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= BPayFormKey0]),
					    
					    io:format("DEBUG>>> billy_bpay_handler:redirector BpayFormData:~p~n" , [BPayFormData]),
					    io:format("DEBUG>>> billy_bpay_handler:redirector BPayFormKey:~p~n" , [BPayFormKey]),
					    
					    {ok, BPayFormHtml} = billy_bpay_dtl:render([{data , BPayFormDataBase64}, {key, BPayFormKey}]),
					    
					    %% Вернуть результат
					    Body = BPayFormHtml,
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



success(_Req) ->
    io:format("DEBUG>>> billy_bpay_handler:success!~n"),
    {redirect, <<"https://gocs.pro">>}.


fail(_Req) ->
    io:format("DEBUG>>> billy_bpay_handler:fail!~n"),
    {redirect, <<"https://gocs.pro">>}.


unknown_method() ->
    io:format("DEBUG>>> billy_bpay_handler:unknown_method!~n"),
    Body = <<"Unknown method">>,
    {output, Body}.









%% Формируем ссылку-редиректор для формирования формы плвтежа в системе bpay
get_payment_link(#{tr_id := TrId, signature := Signature}) ->
    BaseLink = "https://bvvd.ru/payment/bpay/redirector",
    %% BaseLink = "http://127.0.0.1:8008/payment/bpay/redirector",

    A = unicode:characters_to_binary(io_lib:format("~ts?tr_id=~p&signature=~ts",
						   [BaseLink, TrId, Signature])),
    io:format("DEBUG>>> billy_bpay_handler:get_payment_link  A!!!!>>>   ~ts~n", [A]),
    A.



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
    Data = proplists:get_value(data, PostData),
    Key = proplists:get_value(key, PostData),
    
    CheckResult = [
		   {data, billy_query_helper:check_string(Data, 1024)},
		   {key,  billy_query_helper:check_string(Key, 64)}
		  ],
    FinCheck = billy_query_helper:finaly_check(CheckResult),
    
    case FinCheck of
	true  -> {ok, CheckResult};
	false -> {error, CheckResult}
    end.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Служебные функции

xml_val(X) ->
    [#xmlElement{name = _N, content = [#xmlText{value = V}|_]}] = X,
    V.


generate_bpay_formkey(#{bpay_form_data := BPayFormData, bpay_signature := BPaySignature}) ->
    BPayFormKeyA = lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= erlang:md5(BPayFormData)]),
    BPayFormKeyB = lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= erlang:md5(BPaySignature)]),
    BPayFormKey0 = erlang:md5(
		     lists:flatten(
		       io_lib:format("~ts~ts", [BPayFormKeyA, BPayFormKeyB])
		      )
		    ),
    BPayFormKey = lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= BPayFormKey0]),
    BPayFormKey.


check_bpay_formkey(#{bpay_form_data := BPayFormData, bpay_signature := BPaySignature, in_bpay_form_key := InBPayFormKey}) ->
    MyBPayFormKey = generate_bpay_formkey(#{bpay_form_data => BPayFormData, bpay_signature => BPaySignature}),
    io:format("DEBUG>>> billy_bpay_handler:check_bpay_formkey  MyBPayFormKey=~p~n", [MyBPayFormKey]),
    io:format("DEBUG>>> billy_bpay_handler:check_bpay_formkey  InBPayFormKey=~p~n", [InBPayFormKey]),
    case MyBPayFormKey of
	InBPayFormKey -> true;
	_ -> false
    end.






%% success(Req) ->
%%     {ok, ReqParamsKV, _} = cowboy_req:read_urlencoded_body(Req),
%%     io:format("DEBUG>>> billy_bpay_handler:success  ReqParamsKV=~p~n", [ReqParamsKV]),

%%     PostData = [
%% 		{out_sum, proplists:get_value(<<"OutSum">>, ReqParamsKV)},
%% 		{inv_id, proplists:get_value(<<"InvId">>, ReqParamsKV)},
%% 		{signature_value, proplists:get_value(<<"SignatureValue">>, ReqParamsKV)} 
%% 	       ],

%%     case check_post_data(result, PostData) of
%% 	{error, CheckRes} ->
%% 	    {output, <<"ERROR">>};
%% 	{ok, NormPostData} ->

%% 	    OutSumStr = proplists:get_value(<<"OutSum">>, ReqParamsKV),
%% 	    OutSum = proplists:get_value(out_sum, NormPostData),
%% 	    InvId = proplists:get_value(inv_id, NormPostData),
%% 	    MerchantPass = billy_config:get(robokassa_pass1),

%% 	    RobokassaSignParams = #{out_sum => OutSumStr, inv_id => InvId, merchant_pass => MerchantPass},
%% 	    InRobokassaSign = proplists:get_value(signature_value, NormPostData),
	    
%% 	    %% Проверить сигнатуру
%% 	    case check_robokassa_signature(RobokassaSignParams, string:to_upper(InRobokassaSign)) of
%% 		true ->
%% 		    %% получить транзакцию связанную с оплатой
%% 		    TrId = proplists:get_value(inv_id, NormPostData),
%% 		    case billy_cbserver:get_transaction(#{transaction_id => TrId, res_type => json}) of
%% 			{ok, TrProplist} ->
%% 			    io:format("DEBUG>>> billy_robokassa_handler:success TrProplist=~p~n", [TrProplist]),
			    
%% 			    TrParamsBinJson = proplists:get_value(<<"params">>, TrProplist),
%% 			    TrParams = jiffy:decode(TrParamsBinJson, [return_maps]),
			    
%% 			    io:format("DEBUG>>> billy_robokassa_handler:success TrParams=~p~n", [TrParams]),
			    
%% 			    case maps:get(<<"method">>, TrParams, undefined) of
%% 				<<"robokassa">> ->
%% 				    %% TODO :
%% 				    %% Получить данные по транзакции
%% 				    %% Определить, задавались ли URL перенаправления в запросе
%% 				    %% Если нет - получить из данных мерчанта

%% 				    %% Получить данные по мерчанту транзакции
%% 				    TrUserId =  proplists:get_value(<<"user_id">>, TrProplist),
%% 				    case billy_cbserver:get_user(#{user_id => TrUserId, res_type => json}) of
%% 					{ok, [MerchantUserProplist]} ->
					    
%% 					    %% Получить ссылки перенаправления из настроек мерчанта
%% 					    MerchantParamsStr = proplists:get_value(<<"params">>, MerchantUserProplist),
%% 					    MerchantParams = jiffy:decode(MerchantParamsStr, [return_maps]),
%% 					    MerchSuccessUrl = maps:get(<<"success_url">>, MerchantParams),

%% 					    {redirect, MerchSuccessUrl}
%% 				    end;
%% 				_ -> {output, <<"BAD PAYMENT METHOD">>}
%% 			    end
%% 		    end;
%% 		false ->
%% 		    {output, <<"SIGNATURE ERROR">>}
%% 	    end
%%     end.




%% fail(Req) ->
%%     {ok, ReqParamsKV, _} = cowboy_req:read_urlencoded_body(Req),
%%     io:format("DEBUG>>> billy_robokassa_handler:fail  ReqParamsKV=~p~n", [ReqParamsKV]),

%%     PostData = [
%% 		{out_sum, proplists:get_value(<<"OutSum">>, ReqParamsKV)},
%% 		{inv_id, proplists:get_value(<<"InvId">>, ReqParamsKV)},
%% 		{signature_value, <<"">>}
%% 	       ],

%%     case check_post_data(result, PostData) of
%% 	{error, CheckRes} ->
%% 	    {output, <<"ERROR">>};
%% 	{ok, NormPostData} ->

%% 	    InvId = proplists:get_value(inv_id, NormPostData),

%% 	    %% получить транзакцию связанную с оплатой
%% 	    TrId = proplists:get_value(inv_id, NormPostData),
%% 	    case billy_cbserver:get_transaction(#{transaction_id => TrId, res_type => json}) of
%% 		{ok, TrProplist} ->
%% 		    io:format("DEBUG>>> billy_robokassa_handler:fail TrProplist=~p~n", [TrProplist]),
		    
%% 		    TrParamsBinJson = proplists:get_value(<<"params">>, TrProplist),
%% 		    TrParams = jiffy:decode(TrParamsBinJson, [return_maps]),
		    
%% 		    io:format("DEBUG>>> billy_robokassa_handler:fail TrParams=~p~n", [TrParams]),
		    
%% 		    case maps:get(<<"method">>, TrParams, undefined) of
%% 			<<"robokassa">> ->
%% 			    %% TODO :
%% 			    %% Получить данные по транзакции
%% 			    %% Определить, задавались ли URL перенаправления в запросе
%% 			    %% Если нет - получить из данных мерчанта
			    
%% 			    %% Получить данные по мерчанту транзакции
%% 			    TrUserId =  proplists:get_value(<<"user_id">>, TrProplist),
%% 			    case billy_cbserver:get_user(#{user_id => TrUserId, res_type => json}) of
%% 				{ok, [MerchantUserProplist]} ->
					    
%% 				    %% Получить ссылки перенаправления из настроек мерчанта
%% 				    MerchantParamsStr = proplists:get_value(<<"params">>, MerchantUserProplist),
%% 				    MerchantParams = jiffy:decode(MerchantParamsStr, [return_maps]),
%% 				    MerchSuccessUrl = maps:get(<<"fail_url">>, MerchantParams),
				    
%% 				    {redirect, MerchSuccessUrl}
%% 			    end;
%% 			_ -> {output, <<"BAD PAYMENT METHOD">>}
%% 		    end
%% 	    end		
%%     end.
