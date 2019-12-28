-module(billy_robokassa_handler).

-export([init/2]).
-export([get_payment_link/1]).

init(Req, OptsMap) ->
    
    #{method := QueryMethod} = Req,
    #{method := ApiMethod} = OptsMap,

    ApiRes = case ApiMethod of
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



notification(Req) ->
    %% Прочитать параметры и авторизовать платёж
    {ok, ReqParamsKV, _} = cowboy_req:read_urlencoded_body(Req),
    io:format("DEBUG>>> billy_robokassa_handler:notification  ReqParamsKV=~p~n", [ReqParamsKV]),
    
    PostData = [
		{out_sum, proplists:get_value(<<"OutSum">>, ReqParamsKV)},
		{inv_id, proplists:get_value(<<"InvId">>, ReqParamsKV)},
		{signature_value,proplists:get_value(<<"SignatureValue">>, ReqParamsKV)} 
	       ],
    
    case check_post_data(result, PostData) of
	{error, CheckRes} ->
	    {output, <<"ERROR">>};
	{ok, NormPostData} ->

	    OutSumStr = proplists:get_value(<<"OutSum">>, ReqParamsKV),
	    OutSum = proplists:get_value(out_sum, NormPostData),
	    InvId = proplists:get_value(inv_id, NormPostData),

	    %% Получить транзакцию связанную с оплатой
	    {ok, [TrProplist | _]} = billy_transaction:get(#{transaction_id => InvId}),
	    TrParamsStr = proplists:get_value(<<"params">>, TrProplist),
	    TrParamsMap = jiffy:decode(TrParamsStr, [return_maps]),

	    %% Загружаем конфиг мерчанта по платёжной системе
	    MerchantId = proplists:get_value(<<"merchant_id">>, TrProplist),
	    PaySystemKey = maps:get(<<"system">>, TrParamsMap),
	    {ok, PaysystemConfigMap} = billy_config:get(#{merchant_id => MerchantId, paysystem_key => PaySystemKey}), 

	    MerchantPass = maps:get(<<"robokassa_pass2">>, PaysystemConfigMap),

	    %% Проверить сигнатуру
	    LocSignParams = #{out_sum => OutSumStr, inv_id => InvId, merchant_pass => MerchantPass},
	    ReqRobokassaSign = proplists:get_value(signature_value, NormPostData),
	    case check_robokassa_signature(LocSignParams, ReqRobokassaSign) of
		true ->
		    %% Проверить сумму и валюту платежа
		    RoboCost = erlang:round(OutSum * 100),
		    RoboCurrency = <<"RUB">>,

		    TrCost = proplists:get_value(<<"amount">>, TrProplist),
		    TrCurrency = proplists:get_value(<<"ccy_alpha">>, TrProplist),

		    %% Проверить сумму транзакции
		    case {RoboCost, RoboCurrency} of
			{TrCost, TrCurrency} ->
			    ProcessResult = <<"success">>,
			    case billy_payment:process_transaction(#{transaction_id => InvId, process_result => ProcessResult}) of
				ok ->
				    {output, <<"OK">>};
				{error, transaction_already_processed} ->
				    {output, <<"OK">>};
				{error, Reason} ->
				    {output, <<"SYSTEM PROCESS TRANSACTION ERROR">>}
			    end;
			{TrCost, _} -> 
			    {output, <<"CURRENCY ERROR">>};
			{_, TrCurrency} -> 
			    {output, <<"AMOUNT ERROR">>};
			_ ->
			    {output, <<"SYSTEM ERROR">>}
		    end;
		false ->
		    {output, <<"SIGNATURE ERROR">>}
	    end,
	    {output, <<"OK">>}
    end.


success(Req) ->
    {ok, ReqParamsKV, _} = cowboy_req:read_urlencoded_body(Req),
    io:format("DEBUG>>> billy_robokassa_handler:success  ReqParamsKV=~p~n", [ReqParamsKV]),

    PostData = [
		{out_sum, proplists:get_value(<<"OutSum">>, ReqParamsKV)},
		{inv_id, proplists:get_value(<<"InvId">>, ReqParamsKV)},
		{signature_value, proplists:get_value(<<"SignatureValue">>, ReqParamsKV)} 
	       ],

    case check_post_data(result, PostData) of
	{error, CheckRes} ->
	    {output, <<"ERROR">>};
	{ok, NormPostData} ->

	    OutSumStr = proplists:get_value(<<"OutSum">>, ReqParamsKV),
	    OutSum = proplists:get_value(out_sum, NormPostData),
	    InvId = proplists:get_value(inv_id, NormPostData),

	    %% Получить транзакцию связанную с оплатой
	    {ok, [TrProplist | _]} = billy_transaction:get(#{transaction_id => InvId}),
	    TrParamsStr = proplists:get_value(<<"params">>, TrProplist),
	    TrParamsMap = jiffy:decode(TrParamsStr, [return_maps]),	    

	    %% Загружаем конфиг мерчанта по платёжной системе
	    MerchantId = proplists:get_value(<<"merchant_id">>, TrProplist),
	    PaySystemKey = maps:get(<<"system">>, TrParamsMap),
	    {ok, PaysystemConfigMap} = billy_config:get(#{merchant_id => MerchantId, paysystem_key => PaySystemKey}), 

	    MerchantPass = maps:get(<<"robokassa_pass1">>, PaysystemConfigMap),

	    RobokassaSignParams = #{out_sum => OutSumStr, inv_id => InvId, merchant_pass => MerchantPass},
	    InRobokassaSign = proplists:get_value(signature_value, NormPostData),
	    
	    %% Проверить сигнатуру
	    LocSignParams = #{out_sum => OutSumStr, inv_id => InvId, merchant_pass => MerchantPass},
	    ReqRobokassaSign = proplists:get_value(signature_value, NormPostData),
	    case check_robokassa_signature(LocSignParams, string:to_upper(ReqRobokassaSign)) of
		true ->
		    %% Загружаем общий конфиг мерчанта
		    {ok, MerchantConfig} = billy_config:get(#{merchant_id => MerchantId}),

		    %% Получаем ссылку перенаправления
		    {ok, [{_, MerchantSuccessUrl}]} = billy_config:get(#{merchant_config => MerchantConfig, key => "success_url"}),

		    {redirect, MerchantSuccessUrl};

		false ->
		    {output, <<"SIGNATURE ERROR">>}
	    end
    end.




fail(Req) ->
    {ok, ReqParamsKV, _} = cowboy_req:read_urlencoded_body(Req),
    io:format("DEBUG>>> billy_robokassa_handler:fail  ReqParamsKV=~p~n", [ReqParamsKV]),

    PostData = [
		{out_sum, proplists:get_value(<<"OutSum">>, ReqParamsKV)},
		{inv_id, proplists:get_value(<<"InvId">>, ReqParamsKV)},
		{signature_value, <<"">>}
	       ],

    case check_post_data(result, PostData) of
	{error, CheckRes} ->
	    {output, <<"ERROR">>};
	{ok, NormPostData} ->

	    InvId = proplists:get_value(inv_id, NormPostData),

	    %% Получить транзакцию связанную с оплатой
	    {ok, [TrProplist | _]} = billy_transaction:get(#{transaction_id => InvId}),

	    %% Загружаем общий конфиг мерчанта
	    MerchantId = proplists:get_value(<<"merchant_id">>, TrProplist),
	    {ok, MerchantConfig} = billy_config:get(#{merchant_id => MerchantId}),
	    
	    %% Получаем ссылку перенаправления
	    {ok, [{_, MerchantFailUrl}]} = billy_config:get(#{merchant_config => MerchantConfig, key => "fail_url"}),
	    
	    {redirect, MerchantFailUrl}
    end.


unknown_method() ->
    io:format("DEBUG>>> billy_robokassa_handler:unknown_method!~n"),
    Body = <<"Unknown method">>,
    {ok, Body}.



%% Формируем ссылку на оплату в системе QIWI
%% get_payment_link(TransactionId, TrCost, TrCurrency, UserTel, PaySource) ->
get_payment_link(#{tr_id := TrId, tr_cost := TrCost, tr_ccy := TrCurrency, comment := Comment}) ->
    BaseLink = "https://auth.robokassa.ru/Merchant/Index.aspx",
    %% BaseLink = "https://auth.robokassa.ru/Merchant/PaymentForm/FormMS.js",

    %% Отформатировать сумму из копеек в дробь
    TrCostStr = float_to_list(TrCost / 100, [{decimals,2}]),
    TrIdStr = io_lib:format("~p", [TrId]),
    
    SignatureSTR = lists:concat([billy_config:get(robokassa_merchant), ":", TrCostStr, ":", TrIdStr, ":", billy_config:get(robokassa_pass1)]),
    MD5 = erlang:md5(SignatureSTR),
    SignatureValue = string:to_upper(lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= MD5])),

    A = unicode:characters_to_binary(io_lib:format("~ts?MerchantLogin=~ts&OutSum=~ts&InvId=~p&Desc=~ts&SignatureValue=~ts",
						   [BaseLink, billy_config:get(robokassa_merchant), TrCostStr, TrId, Comment, SignatureValue])),

    io:format("DEBUG>>> billy_robokassa_handler:get_payment_link  A!!!!>>>   ~ts~n", [A]),

    A.
%% SAMPLE RedirectUrl
%% https://auth.robokassa.ru/Merchant/Index.aspx?isTest=1&MerchantLogin=bvvd123555&InvId=345&OutSum=100.00&SignatureValue=3aedc7f337c09592dde21db788b13f2e&Culture=ru




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Функции проверки данных POST запросов


check_post_data(result, PostData) ->
    OutSum = proplists:get_value(out_sum, PostData),
    InvId = proplists:get_value(inv_id, PostData),
    SignatureValue = proplists:get_value(signature_value, PostData),
    CheckResult = [
		   {out_sum, billy_query_helper:check_float(OutSum)},
		   {inv_id,  billy_query_helper:check_integer(InvId)},
		   {signature_value,  billy_query_helper:check_string(SignatureValue, 64)}
		  ],
    FinCheck = billy_query_helper:finaly_check(CheckResult),
    
    case FinCheck of
	true  -> {ok, CheckResult};
	false -> {error, CheckResult}
    end.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Служебные функции



generate_robokassa_signature(#{out_sum := OutSum, inv_id := InvId, merchant_pass := MerchantPass}) when 
      (is_list(OutSum) or is_binary(OutSum)) and 
      is_integer(InvId) and
      (is_list(MerchantPass) or is_binary(MerchantPass)) ->

    %% OutSumFormatted = float_to_list(OutSum, [{decimals, 2}]),
    STR = lists:flatten(io_lib:format("~ts:~p:~ts", [OutSum,InvId,MerchantPass])),
    MD5 = erlang:md5(STR),
    SignatureValue = string:to_upper(lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= MD5])),
    
    io:format("DEBUG>>> billy_robokassa_handler: generate_robokassa_signature SignatureValue: ~ts~n", [SignatureValue]),

    UpperSignatureValue = string:to_upper(SignatureValue),
    list_to_binary(UpperSignatureValue).



check_robokassa_signature(RobokassaSignParams, InRobokassaSign) when is_list(InRobokassaSign) ->
    io:format("DEBUG>>> billy_robokassa_handler: generate_robokassa_signature InRobokassaSign: ~ts~n", [InRobokassaSign]),
    MyRobokassaSign = generate_robokassa_signature(RobokassaSignParams),
    InRobokassaSignBin = list_to_binary(InRobokassaSign),
    case MyRobokassaSign of
	InRobokassaSignBin -> true;
	_ -> false
    end;
check_robokassa_signature(RobokassaSignParams, InRobokassaSign) when is_binary(InRobokassaSign) ->
    io:format("DEBUG>>> billy_robokassa_handler: generate_robokassa_signature InRobokassaSign: ~ts~n", [InRobokassaSign]),
    MyRobokassaSign = generate_robokassa_signature(RobokassaSignParams),
    case MyRobokassaSign of
	InRobokassaSign -> true;
	_ -> false
    end.
