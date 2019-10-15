-module(billy_api_masspayments_handler).
-export([init/2]).

init(Req0, OptsMap) ->
    
    #{method := QueryMethod} = Req0,
    #{method := ApiMethod} = OptsMap,

    {ok, Body} = case ApiMethod of
		     get_balance -> get_balance(Req0);
		     check_opportunity -> check_opportunity(Req0);
		     create_transaction -> create_transaction(Req0);
		     _ -> unknown_method()
		 end,
    
    Req = cowboy_req:reply(200,
			   #{<<"content-type">> => <<"text/plain">>},
			   Body,
			   Req0),

    {ok, Req, OptsMap}.



get_balance(Req0) ->

    %% Прочитать параметры и авторизовать платёж
    {ok, ReqParamsKV, _} = cowboy_req:read_urlencoded_body(Req0),

    %% Проверяем полученые данные
    PostData = [
		{system, proplists:get_value(<<"system">>, ReqParamsKV)},
		{system_config_group, proplists:get_value(<<"system_config_group">>, ReqParamsKV)},
		{ccy, proplists:get_value(<<"ccy">>, ReqParamsKV)},
		{billy_public_key, proplists:get_value(<<"billy_public_key">>, ReqParamsKV)}
	       ],
    
    %% Проверяем полученые данные
    case check_post_data(get_balance, PostData) of
	{error, _} ->

	    %% Неверный формат входных данных
	    Responce = {[ {status, error}, {error_code, 1} ]},
	    Body = jiffy:encode(Responce),
	    {ok, Body};

	{ok, NormPostData} ->

	    System = list_to_binary(proplists:get_value(system, NormPostData)),
	    SystemConfigGroup = proplists:get_value(system_config_group, NormPostData),
	    Ccy = proplists:get_value(ccy, NormPostData),
	    BillyPublicKeyQuery = proplists:get_value(billy_public_key, NormPostData),
	    BillyPublicKeyLocal = billy_config:get(billy_public_key),
	    case BillyPublicKeyQuery of
		BillyPublicKeyLocal ->
		    %% Получить модуль-обработчик в зависимости от системы
		    ProcessModule = case System of
					<<"qiwi">> -> billy_mp_qiwi_handler;
					<<"steam">> -> billy_mp_qiwi_handler;
					<<"payeer">> -> billy_mp_payeer_handler;
					<<"btc">> -> billy_mp_btc_handler
				    end,

		    case ProcessModule:get_balance(#{ccy_alpha => Ccy, config_group_id => SystemConfigGroup}) of
			%% Данные получены
			{ok, BalanceInfoMap} ->
			    
			    #{balance := Balance, system_wallet := SystemWallet} = BalanceInfoMap,

			    Responce = {[ {status, ok}, {balance, Balance}, {system_wallet, SystemWallet} ]},
			    Body = jiffy:encode(Responce),
			    {ok, Body}
		    end;
		_ ->
		    %% Неверный публичный ключ
		    Responce = {[ {status, error}, {error_code, 2} ]},
		    Body = jiffy:encode(Responce),
		    {ok, Body}
	    end
    end.


check_opportunity(Req0) ->

    %% Прочитать параметры и авторизовать платёж
    {ok, ReqParamsKV, _} = cowboy_req:read_urlencoded_body(Req0),
    
    ReqCcy = proplists:get_value(<<"ccy">>, ReqParamsKV),

    %% Проверяем полученые данные
    PostData = [
		{merchant_bill_id, proplists:get_value(<<"merchant_bill_id">>, ReqParamsKV)},

		{amount, proplists:get_value(<<"amount">>, ReqParamsKV)},
		{ccy, ReqCcy},
		{method, proplists:get_value(<<"method">>, ReqParamsKV)},
		{method_config_group, proplists:get_value(<<"method_config_group">>, ReqParamsKV)},
		{address, proplists:get_value(<<"address">>, ReqParamsKV)},
		{desc, proplists:get_value(<<"desc">>, ReqParamsKV)},

		{merchant_id, proplists:get_value(<<"merchant_id">>, ReqParamsKV)},
		{public_key, proplists:get_value(<<"public_key">>, ReqParamsKV)},
		{signature, proplists:get_value(<<"signature">>, ReqParamsKV)}
	       ],
    
    %% Проверяем полученые данные
    case check_post_data(check_opportunity, PostData) of
	{error, _} ->

	    %% Неверный формат входных данных
	    Responce = {[ {status, error}, {error_code, 1} ]},
	    Body = jiffy:encode(Responce),
	    {ok, Body};

	{ok, NormPostData} ->

	    %% io:format("DEBUG>>> billy_masspayment_handler:create_transaction  NormPostData=~p~n", [NormPostData]),

	    MerchantBillId = proplists:get_value(merchant_bill_id, NormPostData),
	    Amount = proplists:get_value(amount, NormPostData),

	    %% Получить код валюты на основе 3х буквенного кода (iso4107)
	    Ccy = list_to_binary(proplists:get_value(ccy, NormPostData)),
	    CcyNumber = billy_commons:get_currency_number(#{currency_alpha => Ccy}),

	    Method = list_to_binary(proplists:get_value(method, NormPostData)),
	    MethodConfigGroup = proplists:get_value(method_config_group, NormPostData),

	    Address = list_to_binary(proplists:get_value(address, NormPostData)),

	    Desc = proplists:get_value(desc, NormPostData),

	    MerchantId = proplists:get_value(merchant_id, NormPostData),
	    PublicKey = list_to_binary(proplists:get_value(public_key, NormPostData)),
	    Signature = proplists:get_value(signature, NormPostData),

	    case billy_cbserver:get_user(#{user_id => MerchantId, res_type => json}) of
		{ok, [MerchantUserProplist]} ->

		    MerchantName = proplists:get_value(<<"name">>, MerchantUserProplist),

		    %% Проверить публичный ключ
		    MerchantParamsStr = proplists:get_value(<<"params">>, MerchantUserProplist),
		    MerchantParams = jiffy:decode(MerchantParamsStr, [return_maps]),
		    MerchantPublicKey = maps:get(<<"public_key">>, MerchantParams),
		    case MerchantPublicKey of
			PublicKey ->
			    %% Получаем секретный ключ мерчанта
			    MerchantSecretKey = maps:get(<<"secret_key">>, MerchantParams),
			    
			    %% Формируем сигнатуру из параметров
			    BillySignParams = #{bill_id => MerchantBillId, amount => Amount, ccy => Ccy, merchant_secret_key => MerchantSecretKey},
			    QuerySignVal = billy_commons:get_billy_signature(BillySignParams),

			    %% io:format("DEBUG>>> billy_masspayment_handler:create_transaction  Signs=~p:~p~n", [Signature, QuerySignVal]),

			    %% Проверяем сигнатуру по закрытому ключу
			    case QuerySignVal of
				Signature ->
				    
				    %% Вызвать ф-ю проверки возможности отправки у соотв. системы

				    %% Получить модуль-обработчик в зависимости от метода
				    ProcessModule = case Method of
							<<"qiwi">> -> billy_mp_qiwi_handler;
							<<"steam">> -> billy_mp_qiwi_handler;
							<<"payeer">> -> billy_mp_payeer_handler;
							<<"btc">> -> billy_mp_btc_handler
						    end,
				    
				    case ProcessModule:check_opportunity(#{amount => Amount, ccy_alpha => Ccy, address => Address, config_group_id => MethodConfigGroup}) of
					%% Отправка возможна
					ok ->
					    Responce = {[ {status, ok} ]},
					    Body = jiffy:encode(Responce),
					    {ok, Body};

					%% Не хватает средств
					{error, low_balance} ->
					    Responce = {[ {status, error}, {error_code, 8} ]},
					    Body = jiffy:encode(Responce),
					    {ok, Body};

					%% Неправильный адрес
					{error, incorrect_address} ->
					    Responce = {[ {status, error}, {error_code, 15} ]},
					    Body = jiffy:encode(Responce),
					    {ok, Body}
				    end;
				
				QuerySignVal ->
				    %% Сигнатура неверна
				    Responce = {[ {status, error}, {error_code, 3} ]},
				    Body = jiffy:encode(Responce),
				    {ok, Body}
			    end;
			_ ->
			    %% Неверный публичный ключ
			    Responce = {[ {status, error}, {error_code, 2} ]},
			    Body = jiffy:encode(Responce),
			    {ok, Body}
		    end
	    end
    end.



create_transaction(Req0) ->

    %% Прочитать параметры и авторизовать платёж
    {ok, ReqParamsKV, _} = cowboy_req:read_urlencoded_body(Req0),
    
    ReqCcy = proplists:get_value(<<"ccy">>, ReqParamsKV),

    %% Проверяем полученые данные
    PostData = [
		{merchant_bill_id, proplists:get_value(<<"merchant_bill_id">>, ReqParamsKV)},

		{amount, proplists:get_value(<<"amount">>, ReqParamsKV)},
		{ccy, ReqCcy},
		{method, proplists:get_value(<<"method">>, ReqParamsKV)},
		{method_config_group, proplists:get_value(<<"method_config_group">>, ReqParamsKV)},
		{address, proplists:get_value(<<"address">>, ReqParamsKV)},
		{desc, proplists:get_value(<<"desc">>, ReqParamsKV)},

		{merchant_id, proplists:get_value(<<"merchant_id">>, ReqParamsKV)},
		{public_key, proplists:get_value(<<"public_key">>, ReqParamsKV)},
		{signature, proplists:get_value(<<"signature">>, ReqParamsKV)}
	       ],
    
    %% Проверяем полученые данные
    case check_post_data(create_transaction, PostData) of
	{error, _} ->

	    %% Неверный формат входных данных
	    Responce = {[ {status, error}, {error_code, 1} ]},
	    Body = jiffy:encode(Responce),
	    {ok, Body};

	{ok, NormPostData} ->

	    %% io:format("DEBUG>>> billy_masspayment_handler:create_transaction  NormPostData=~p~n", [NormPostData]),

	    MerchantBillId = proplists:get_value(merchant_bill_id, NormPostData),
	    Amount = proplists:get_value(amount, NormPostData),

	    %% Получить код валюты на основе 3х буквенного кода (iso4107)
	    Ccy = list_to_binary(proplists:get_value(ccy, NormPostData)),
	    CcyNumber = billy_commons:get_currency_number(#{currency_alpha => Ccy}),

	    Method = list_to_binary(proplists:get_value(method, NormPostData)),
	    MethodConfigGroup = proplists:get_value(method_config_group, NormPostData),

	    Address = list_to_binary(proplists:get_value(address, NormPostData)),

	    Desc = proplists:get_value(desc, NormPostData),

	    MerchantId = proplists:get_value(merchant_id, NormPostData),
	    PublicKey = list_to_binary(proplists:get_value(public_key, NormPostData)),
	    Signature = proplists:get_value(signature, NormPostData),

	    case billy_cbserver:get_user(#{user_id => MerchantId, res_type => json}) of
		{ok, [MerchantUserProplist]} ->

		    MerchantName = proplists:get_value(<<"name">>, MerchantUserProplist),

		    %% TODO : Это говно надо переделать на RSA

		    %% Проверить публичный ключ
		    MerchantParamsStr = proplists:get_value(<<"params">>, MerchantUserProplist),
		    MerchantParams = jiffy:decode(MerchantParamsStr, [return_maps]),
		    MerchantPublicKey = maps:get(<<"public_key">>, MerchantParams),
		    case MerchantPublicKey of
			PublicKey ->
			    %% Получаем секретный ключ мерчанта
			    MerchantSecretKey = maps:get(<<"secret_key">>, MerchantParams),
			    
			    %% Формируем сигнатуру из параметров
			    BillySignParams = #{bill_id => MerchantBillId, amount => Amount, ccy => Ccy, merchant_secret_key => MerchantSecretKey},
			    QuerySignVal = billy_commons:get_billy_signature(BillySignParams),

			    %% io:format("DEBUG>>> billy_masspayment_handler:create_transaction  Signs=~p:~p~n", [Signature, QuerySignVal]),

			    %% Проверяем сигнатуру по закрытому ключу
			    case QuerySignVal of
				Signature ->
				    
				    %% Подсчитать хэши метода и адреса отправки
				    <<MethodHash:64, _/binary>> = crypto:hash(sha, Method),
				    <<AddressHash:64, _/binary>> = crypto:hash(sha, Address),
				    
				    %% Создаём заказ на отправку средств
				    CreateOrderParams = [MerchantId, MerchantBillId, Amount, Ccy, CcyNumber, Method, MethodHash, Address, AddressHash, MethodConfigGroup, calendar:local_time(), 0],
				    case billy_mysql:exec_prepared_stmt(#{stmt => create_masspayment_order_stmt, params => CreateOrderParams}) of
					{ok, OrderId} ->					    
				    	    Responce = {[ {status, ok}, {order_id, OrderId} ]},
				    	    Body = jiffy:encode(Responce),
				    	    {ok, Body}
				    end;
				QuerySignVal ->
				    %% Сигнатура неверна
				    Responce = {[ {status, error}, {error_code, 3} ]},
				    Body = jiffy:encode(Responce),
				    {ok, Body}
			    end;
			_ ->
			    %% Неверный публичный ключ
			    Responce = {[ {status, error}, {error_code, 2} ]},
			    Body = jiffy:encode(Responce),
			    {ok, Body}
		    end
	    end
    end.



unknown_method() ->
    io:format("DEBUG>>> billy_masspayment_handler:unknown_method!~n"),
    Body = <<"Unknown method">>,
    {ok, Body}.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Функции проверки данных POST запросов


check_post_data(get_balance, PostData) ->
    SSystem = proplists:get_value(system, PostData),
    SSystemConfigGroup = proplists:get_value(system_config_group, PostData),
    SCcy = proplists:get_value(ccy, PostData),
    SBillyPublicKey = proplists:get_value(billy_public_key, PostData),
    CheckResult = [
		   {system, billy_query_helper:check_string(SSystem, 20)},
		   {system_config_group, billy_query_helper:check_integer(SSystemConfigGroup)},
		   {ccy, billy_query_helper:check_string(SCcy, 3)},
		   {billy_public_key, billy_query_helper:check_string(SBillyPublicKey, 64)}
		  ],
    FinCheck = billy_query_helper:finaly_check(CheckResult),

    case FinCheck of
	true  -> {ok, CheckResult};
	false -> {error, CheckResult}
    end;


check_post_data(check_opportunity, PostData) ->
    SMerchantBillId = proplists:get_value(merchant_bill_id, PostData),

    SAmount = proplists:get_value(amount, PostData),
    SCcy = proplists:get_value(ccy, PostData),
    SMethod = proplists:get_value(method, PostData),
    SMethodConfigGroup = proplists:get_value(method_config_group, PostData),
    SAddress = proplists:get_value(address, PostData),
    SDesc = proplists:get_value(desc, PostData),

    SMerchantId = proplists:get_value(merchant_id, PostData),
    SPublicKey = proplists:get_value(public_key, PostData),
    SSignature = proplists:get_value(signature, PostData),
    
    CheckResult = [
		   {merchant_bill_id, billy_query_helper:check_integer(SMerchantBillId)},

		   {amount, billy_query_helper:check_integer(SAmount)},
		   {ccy, billy_query_helper:check_string(SCcy, 3)},
		   {method, billy_query_helper:check_string(SMethod, 20)},
		   {method_config_group, billy_query_helper:check_integer(SMethodConfigGroup)},
		   {address, billy_query_helper:check_string(SAddress, 1024)},
		   {desc, billy_query_helper:check_string(SDesc, 128)},

		   {merchant_id, billy_query_helper:check_integer(SMerchantId)},
		   {public_key, billy_query_helper:check_string(SPublicKey, 64)},
		   {signature, billy_query_helper:check_string(SSignature, 128)}
		  ],

    FinCheck = billy_query_helper:finaly_check(CheckResult),

    case FinCheck of
	true  -> {ok, CheckResult};
	false -> {error, CheckResult}
    end;

check_post_data(create_transaction, PostData) ->
    SMerchantBillId = proplists:get_value(merchant_bill_id, PostData),

    SAmount = proplists:get_value(amount, PostData),
    SCcy = proplists:get_value(ccy, PostData),
    SMethod = proplists:get_value(method, PostData),
    SMethodConfigGroup = proplists:get_value(method_config_group, PostData),
    SAddress = proplists:get_value(address, PostData),
    SDesc = proplists:get_value(desc, PostData),

    SMerchantId = proplists:get_value(merchant_id, PostData),
    SPublicKey = proplists:get_value(public_key, PostData),
    SSignature = proplists:get_value(signature, PostData),
    
    CheckResult = [
		   {merchant_bill_id, billy_query_helper:check_integer(SMerchantBillId)},

		   {amount, billy_query_helper:check_integer(SAmount)},
		   {ccy, billy_query_helper:check_string(SCcy, 3)},
		   {method, billy_query_helper:check_string(SMethod, 20)},
		   {method_config_group, billy_query_helper:check_integer(SMethodConfigGroup)},
		   {address, billy_query_helper:check_string(SAddress, 1024)},
		   {desc, billy_query_helper:check_string(SDesc, 128)},

		   {merchant_id, billy_query_helper:check_integer(SMerchantId)},
		   {public_key, billy_query_helper:check_string(SPublicKey, 64)},
		   {signature, billy_query_helper:check_string(SSignature, 128)}
		  ],

    FinCheck = billy_query_helper:finaly_check(CheckResult),

    case FinCheck of
	true  -> {ok, CheckResult};
	false -> {error, CheckResult}
    end.
