-module(billy_api_handler).
-export([init/2]).

-include("ephp.hrl").

init(Req, OptsMap) ->
    
    #{method := QueryMethod} = Req,
    #{method := ApiMethod} = OptsMap,

    ApiCallResult = case ApiMethod of
			create_payment -> create_payment(Req);
			create_transaction -> create_transaction(Req);
			process_transaction -> process_transaction(Req);
			currency_rate -> currency_rate(Req);
			_ -> unknown_method()
		    end,
    
    Resp = case ApiCallResult of
	       {ok, HttpErrorCode, RespHeaders, RespBody} ->
		   cowboy_req:reply(HttpErrorCode, RespHeaders, RespBody, Req);
	       {ok, HttpErrorCode, RespBody} ->
		   cowboy_req:reply(HttpErrorCode, #{}, RespBody, Req);
	       {ok, 200, RespBody} ->
		   DefaultHeareds_200 = #{<<"content-type">> => <<"text/plain">>},
		   cowboy_req:reply(200, DefaultHeareds_200, RespBody, Req);
	       {ok, 500} -> 
		   cowboy_req:reply(500, Req)
	   end,

    {ok, Resp, OptsMap}.


create_transaction(Req0) ->
    io:format("DEBUG>>> billy_api_handler:create_transaction  Req0=~p~n", [Req0]),

    %% Прочитать параметры и авторизовать платёж
    {ok, ReqParamsKV, _} = cowboy_req:read_urlencoded_body(Req0),
    io:format("DEBUG>>> billy_api_handler:create_transaction  ReqParamsKV=~p~n", [ReqParamsKV]),
    
    ReqCcy = proplists:get_value(<<"ccy">>, ReqParamsKV),

    %% Проверяем полученые данные
    PostData = [
		{merchant_id, proplists:get_value(<<"merchant_id">>, ReqParamsKV)},
		{public_key, proplists:get_value(<<"public_key">>, ReqParamsKV)},
		{amount, proplists:get_value(<<"amount">>, ReqParamsKV)},
		{ccy, ReqCcy},
		{bill_id, proplists:get_value(<<"bill_id">>, ReqParamsKV)},
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

	    io:format("DEBUG>>> billy_api_handler:create_transaction  NormPostData=~p~n", [NormPostData]),

	    MerchantId = proplists:get_value(merchant_id, NormPostData),
	    PublicKey = list_to_binary(proplists:get_value(public_key, NormPostData)),

	    Amount = proplists:get_value(amount, NormPostData),

	    %% Получить код валюты на основе 3х буквенного кода (iso4107)
	    Ccy = list_to_binary(proplists:get_value(ccy, NormPostData)),
	    CcyNumber = billy_commons:get_currency_number(#{currency_alpha => Ccy}),

	    BillId = proplists:get_value(bill_id, NormPostData),
	    Signature = proplists:get_value(signature, NormPostData),


	    case billy_cbserver:get_user(#{user_id => MerchantId, res_type => json}) of
		{ok, [MerchantUserProplist]} ->

		    MerchantName = proplists:get_value(<<"name">>, MerchantUserProplist),

		    %% Проверить публичный ключ
		    MerchantParamsStr = proplists:get_value(<<"params">>, MerchantUserProplist),
		    MerchantParams = jiffy:decode(MerchantParamsStr, [return_maps]),

		    io:format("DEBUG>>> billy_api_handler:create_transaction  MerchantParams=~p~n", [MerchantParams]),

		    MerchantPublicKey = maps:get(<<"public_key">>, MerchantParams),

		    case MerchantPublicKey of
			PublicKey ->
			    %% Получаем секретный ключ мерчанта
			    MerchantSecretKey = maps:get(<<"secret_key">>, MerchantParams),
			    
			    %% Формируем сигнатуру из параметров
			    BillySignParams = #{bill_id => BillId, amount => Amount, ccy => Ccy, merchant_secret_key => MerchantSecretKey},
			    QuerySignVal = billy_commons:get_billy_signature(BillySignParams),

			    io:format("DEBUG>>> billy_api_handler:create_transaction  Signs=~p:~p~n", [Signature, QuerySignVal]),

			    %% Проверяем сигнатуру по закрытому ключу
			    case QuerySignVal of
				Signature ->
				    %% Формируем параметры транзакции
				    FinTrParams = [
						   {bill_id, BillId}
						  ],

				    TrParamsErlJson = {FinTrParams},
				    TrParamsStr = jiffy:encode(TrParamsErlJson),

				    io:format("DEBUG>>> billy_api_handler:create_transaction  TrParamsStr=~p~n", [TrParamsStr]),

				    TrCreateMap = #{
				      type => 1, %% 1=сквозная транзакция
				      params => TrParamsStr,
				      user_id => MerchantId,

				      currency_alpha => Ccy,
				      currency_number => CcyNumber,

				      cost => Amount,
				      new_balance => 0,
				      status => 0,
				      create_date => calendar:local_time(),
				      close_date => null
				     },
				    
				    %% Создать транзакцию в cbserver
				    case billy_cbserver:create_transaction(TrCreateMap) of
					{ok, BillyTrId} ->
					    Responce = {[ {status, ok}, {tr_id, BillyTrId} ]},
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


process_transaction(Req0) ->
    io:format("DEBUG>>> billy_api_handler:create_payment  Req0=~p~n", [Req0]),
    ok.


create_payment(Req0) ->

    io:format("DEBUG>>> billy_api_handler:create_payment  Req0=~p~n", [Req0]),

    QueryHost = cowboy_req:header(<<"host">>, Req0),

    io:format("DEBUG>>> billy_api_handler:create_payment  QueryHost=~p~n", [QueryHost]),

    %% Прочитать параметры и авторизовать платёж
    {ok, ReqParamsKV, _} = cowboy_req:read_urlencoded_body(Req0),
    io:format("DEBUG>>> billy_api_handler:create_payment  ReqParamsKV=~p~n", [ReqParamsKV]),
    
    ReqCcy = proplists:get_value(<<"ccy">>, ReqParamsKV),

    %% Проверяем полученые данные
    PostData = [
		{amount, proplists:get_value(<<"amount">>, ReqParamsKV)},
		{ccy, ReqCcy},
		{payee_ccy, proplists:get_value(<<"payee_ccy">>, ReqParamsKV, ReqCcy)},
		{system, proplists:get_value(<<"system">>, ReqParamsKV)},
		{system_params, proplists:get_value(<<"system_params">>, ReqParamsKV)},
		{merchant_id, proplists:get_value(<<"merchant_id">>, ReqParamsKV)},
		{bill_id, proplists:get_value(<<"bill_id">>, ReqParamsKV)},
		{desc, proplists:get_value(<<"desc">>, ReqParamsKV)},
		{anti_fraud_params, proplists:get_value(<<"anti_fraud_params">>, ReqParamsKV)},
		{signature, proplists:get_value(<<"signature">>, ReqParamsKV)}
	       ],
    case check_post_data(create_payment, PostData) of
	{error, _} ->

	    %% Неверный формат входных данных
	    Response = #{status => error, error_code => 1},
	    ResponseBody = jiffy:encode(Response),
	    {ok, 200, #{<<"content-type">> => <<"application/json">>}, ResponseBody};

	{ok, NormPostData} ->

	    io:format("DEBUG>>> billy_api_handler:create_payment  NormPostData=~p~n", [NormPostData]),

	    Amount = proplists:get_value(amount, NormPostData),
	    CcyAlpha = list_to_binary(proplists:get_value(ccy, NormPostData)),
	    PayeeCcy = list_to_binary(proplists:get_value(payee_ccy, NormPostData)),
	    System = list_to_binary(proplists:get_value(system, NormPostData)),
	    SystemParamsBase64 = proplists:get_value(system_params, NormPostData),

	    MerchantId = proplists:get_value(merchant_id, NormPostData),
	    BillId = proplists:get_value(bill_id, NormPostData),
	    Desc = proplists:get_value(desc, NormPostData),
	    AntiFraudParamsBase64 = proplists:get_value(anti_fraud_params, NormPostData),
	    QuerySignature = proplists:get_value(signature, NormPostData),

	    %% Получаем описание мерчанта
	    case billy_merchant:get(#{merchant_id => MerchantId}) of
		{ok, [MerchantUserProplist]} ->

		    %% Загружаем конфиг мерчанта
		    {ok, MerchantConfig} = billy_config:get(#{merchant_id => MerchantId}),

		    %% Получаем секретный ключ мерчанта
		    {ok, [{_, MerchantSecretKey}]} = billy_config:get(#{merchant_config => MerchantConfig, key => "secret_key"}),
		    
		    %% Считаем сигнатуру из параметров
		    BillySignParams = #{bill_id => BillId, amount => Amount, ccy => CcyAlpha, merchant_secret_key => MerchantSecretKey},
		    CountedSignature = billy_commons:get_billy_signature(BillySignParams),

		    io:format("DEBUG>>> billy_api_handler:create_payment  Signs=~p:~p~n", [QuerySignature, CountedSignature]),

		    if
			%% Сигнатуры сходятся
			QuerySignature == CountedSignature ->

			    %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
			    %% Сформировать параметры создания транзакции

			    %% Декодировать base64 параметры выбранной платёжной системы из запроса
			    SystemParamsStr = base64:decode(SystemParamsBase64),
			    SystemParamsMap = jiffy:decode(SystemParamsStr, [return_maps]),
			    
			    %% Декодировать base64 антифрод параметры из запроса
			    AntiFraudParamsStr = base64:decode(AntiFraudParamsBase64),
			    AntiFraudParamsMap = jiffy:decode(AntiFraudParamsStr, [return_maps]),
			    
			    %% Получить код валюты на основе 3х буквенного кода (iso4107)
			    CcyNumber = billy_commons:get_currency_number(#{currency_alpha => CcyAlpha}),
			    
			    %% Считаем сумму платежа в валюте обеспечения
			    {ok, CcyRateMap} = billy_cbr_rate:get_rate(#{currency_alpha => CcyAlpha}),
			    {ok, PayeeCcyRateMap} = billy_cbr_rate:get_rate(#{currency_alpha => PayeeCcy}),
			    
			    io:format("DEBUG>>> billy_api_handler:create_payment  CcyRateMap=~p~n", [CcyRateMap]),
			    io:format("DEBUG>>> billy_api_handler:create_payment  PayeeCcyRateMap=~p~n", [PayeeCcyRateMap]),

			    CcyRateNominal = maps:get(nominal, CcyRateMap),
			    CcyRateValue = maps:get(value, CcyRateMap),

			    PayeeCcyRateNominal = maps:get(nominal, PayeeCcyRateMap),
			    PayeeCcyRateValue = maps:get(value, PayeeCcyRateMap),
			    
			    RubCost = Amount * (CcyRateValue / CcyRateNominal),
			    PayeeRubCost = Amount * (PayeeCcyRateValue / PayeeCcyRateNominal),
			    FinRate = RubCost / PayeeRubCost,

			    PayeeCost = round(Amount * FinRate),

			    %% round((RubCost * PayeeCcyRateNominal) / PayeeCcyRateValue ),

			    %% Формируем параметры транзакции
			    FinTrParamsMap = #{
			      bill_id => BillId, system => System, system_params => SystemParamsMap, anti_fraud_params => AntiFraudParamsMap,
			      rate => FinRate,
			      payee_ccy => PayeeCcy, 
			      payee_cost => PayeeCost
			     },
			    
			    TrParamsStr = jiffy:encode(FinTrParamsMap),

			    TrCreateMap = #{
			      merchant_id => MerchantId,
			      type => 1,
			      amount => Amount,
			      ccy_alpha => CcyAlpha,
			      ccy_number => CcyNumber,
			      params => TrParamsStr
			     },
			    
			    io:format("DEBUG>>> billy_api_handler:create_payment  TrCreateMap=~p~n", [TrCreateMap]),

			    case billy_transaction:create(TrCreateMap) of
				{ok, BillyTrId} ->

				    %% Получить ссылку на оплату в сторонней системе и сформировать ответ
				    case get_payment_link(#{system => System, transaction_id => BillyTrId}) of
					{ok, PaymentLink} ->
					    ResponseBody = jiffy:encode(#{status => ok, redirect_url => PaymentLink}),
					    {ok, 200, #{<<"content-type">> => <<"application/json">>}, ResponseBody};

					{error, eval_php_error, ErrorMessage} ->
					    {ok, 500, ErrorMessage};

					{catch_error, _, _} -> 
					    {ok, 500};
					
					{error, ErrorMessage} ->
					    ResponseBody = jiffy:encode(#{status => error, message => ErrorMessage}),
					    {ok, 200, #{<<"content-type">> => <<"application/json">>}, ResponseBody};
					
					_ ->
					    {ok, 500}
				    end
			    end;

			%% else (Сигнатура неверна)
			true ->
			    Response = #{status => error, error_code => 3},
			    ResponseBody = jiffy:encode(Response),
			    {ok, 200, #{<<"content-type">> => <<"application/json">>}, ResponseBody}
		    end
	    end
    end.



%% RedirectUrl = case System of

%% 		      <<"bpay">> ->

%% 			  GetPLinkParams = #{tr_id => BillyTrId, signature => Signature},
%% 			  billy_bpay_handler:get_payment_link(GetPLinkParams);

%% 		      <<"g2a">> ->
%% 			  GetPLinkParams = #{tr_id => BillyTrId,
%% 					     tr_cost => Amount, 
%% 					     tr_ccy => Ccy,
%% 					     signature => Signature},
%% 			  billy_g2a_handler:get_payment_link(GetPLinkParams);

%% 		      <<"robokassa">> ->

%% 			  Comment0 = http_uri:encode(io_lib:format("Заказ #~p (Игра ~ts)", [BillyTrId, MerchantName])),
%% 			  Comment = unicode:characters_to_binary(Comment0, utf8),

%% 			  GetPLinkParams = #{tr_id => BillyTrId,
%% 					     tr_cost => Amount, 
%% 					     tr_ccy => Ccy,
%% 					     comment => Comment
%% 					    },
%% 			  billy_robokassa_handler:get_payment_link(GetPLinkParams);

%% 		      <<"qiwi">> ->
%% 			  %% Читаем телефон из данных запроса
%% 			  %% !!! Это костыль, надо доделать проверку на чеке
%% 			  %% и читать данные из нормированых параметров
%% 			  Phone0 = proplists:get_value(<<"phone">>, MethodParamsProplist),
%% 			  Phone = io_lib:format("+~ts", [Phone0]),

%% 			  Comment0 = io_lib:format("Заказ #~p (Игра ~ts)", [BillyTrId, MerchantName]),
%% 			  Comment = unicode:characters_to_binary(Comment0, utf8),

%% 			  GetPLinkParams = #{tr_id => BillyTrId,
%% 					     tr_cost => Amount,
%% 					     tr_ccy => Ccy,
%% 					     phone => Phone0,
%% 					     comment => Comment,
%% 					     pay_source => "qw"
%% 					    },
%% 			  billy_qiwi_handler:get_payment_link(GetPLinkParams);

%% 		      <<"qiwicards">> ->
%% 			  %% Читаем телефон из данных запроса
%% 			  %% !!! Это костыль, надо доделать проверку на чеке
%% 			  %% и читать данные из нормированых параметров
%% 			  Phone0 = proplists:get_value(<<"phone">>, MethodParamsProplist),
%% 			  Phone = io_lib:format("+~ts", [Phone0]),

%% 			  Comment0 = io_lib:format("Заказ #~p (Игра ~ts)", [BillyTrId, MerchantName]),
%% 			  Comment = unicode:characters_to_binary(Comment0, utf8),

%% 			  GetPLinkParams = #{tr_id => BillyTrId,
%% 					     tr_cost => Amount,
%% 					     tr_ccy => Ccy,
%% 					     phone => Phone0,
%% 					     comment => Comment,
%% 					     pay_source => "card"
%% 					    },
%% 			  billy_qiwi_handler:get_payment_link(GetPLinkParams);

%% 		      <<"cloudpayments">> ->
%% 			  BaseLink = "https://somacase.com/payment/cloudpayments/checkout",
%% 			  %% BaseLink = "https://188.35.184.37:8008/payment/cloudpayments/checkout",
%% 			  A = unicode:characters_to_binary(io_lib:format("~ts?tr_id=~p&signature=~ts",
%% 									 [BaseLink, BillyTrId, Signature])),
%% 			  io:format("DEBUG>>> cloudpayments link >>>   ~ts~n", [A]),
%% 			  A;

%% 		      <<"connectum">> ->
%% 			  Email = proplists:get_value(<<"email">>, MethodParamsProplist),

%% 			  %% BaseLink = "http://127.0.0.1:8008/payment/connectum/create",
%% 			  BaseLink = "https://somacase.com/payment/connectum/create",
%% 			  A = unicode:characters_to_binary(io_lib:format("~ts?tr_id=~p&signature=~ts",
%% 									 [BaseLink, BillyTrId, Signature])),
%% 			  io:format("DEBUG>>> connectum link >>>   ~ts~n", [A]),
%% 			  A;

%% 		      <<"cauri">> ->
%% 			  Email = proplists:get_value(<<"email">>, MethodParamsProplist),

%% 			  %% BaseLink = "http://127.0.0.1:8008/payment/cauri/checkout",
%% 			  BaseLink = "https://somacase.com/payment/cauri/checkout",
%% 			  A = unicode:characters_to_binary(io_lib:format("~ts?tr_id=~p&signature=~ts",
%% 									 [BaseLink, BillyTrId, Signature])),
%% 			  io:format("DEBUG>>> cauri link >>>   ~ts~n", [A]),
%% 			  A;

%% 		      <<"payeer">> ->
%% 			  BaseLink = "https://bvvd.ru/payment/payeer/checkout",
%% 			  A = unicode:characters_to_binary(io_lib:format("~ts?tr_id=~p&signature=~ts",
%% 									 [BaseLink, BillyTrId, Signature])),
%% 			  io:format("DEBUG>>> payeer link >>>   ~ts~n", [A]),
%% 			  A;

%% 		      <<"bitpay">> ->

%% 			  NotificationUrl = "",
%% 			  %% RedirectUrl = "",

%% 			  GetPLinkParams = #{tr_id => BillyTrId,
%% 					     tr_cost => Amount,
%% 					     tr_ccy => Ccy
%% 					    },
%% 			  billy_bitpay_handler:get_payment_link(GetPLinkParams);

%% 		      <<"skinpay">> ->
%% 			  PartnerId = proplists:get_value(<<"partner_id">>, ReqParamsKV),
%% 			  Token = proplists:get_value(<<"token">>, ReqParamsKV),
%% 			  SecretKey = "cpk123oci21j389xmy128ny38127",
%% 			  SignatureSTR = lists:concat(io_lib:format("~ts~ts~ts~ts", [PartnerId, Token, Ccy, SecretKey])),

%% 			  io:format("DEBUG>>> skinpay SignatureSTR >>>   ~ts~n", [SignatureSTR]),

%% 			  MD5 = erlang:md5(SignatureSTR),
%% 			  SkinpaySign = lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= MD5]),

%% 			  BaseLink = "https://skinpay.gocs.pro/deposit",
%% 			  A = unicode:characters_to_binary(io_lib:format("~ts?partner_id=~ts&token=~ts&gw_transaction_id=~p&currency=~ts&sign=~ts&locale=ru",
%% 									 [BaseLink, PartnerId, Token, BillyTrId, Ccy, SkinpaySign])),
%% 			  io:format("DEBUG>>> skinpay link >>>   ~ts~n", [A]),
%% 			  A
%% 		  end,

%% Получить мерчанта. Возвращаем результат в map,
%% а не в виде рекорда..надоело копировать рекорд + так универсальнее
%% если res_type => map, то billy_cbserver вернёт map, вместо структуры
%% case billy_cbserver:get_user(#{user_id => MerchantId, res_type => json}) of
%% 	{ok, [MerchantUserProplist]} ->

%% 	    MerchantName = proplists:get_value(<<"name">>, MerchantUserProplist),

%% 	    MerchantParamsStr = proplists:get_value(<<"params">>, MerchantUserProplist),
%% 	    MerchantParams = jiffy:decode(MerchantParamsStr, [return_maps]),

%% 	    %% Получаем секретный ключ мерчанта
%% 	    MerchantSecretKey = maps:get(<<"secret_key">>, MerchantParams),

%% 	    %% Формируем сигнатуру из параметров
%% 	    BillySignParams = #{bill_id => BillId, amount => Amount, ccy => Ccy, merchant_secret_key => MerchantSecretKey},
%% 	    QuerySignVal = billy_commons:get_billy_signature(BillySignParams),
%% 	    %% QuerySignStr = io_lib:format("~p~p~ts~ts", [BillId, Amount, Ccy, MerchantSecretKey]),
%% 	    %% QuerySign = crypto:hash(sha256, QuerySignStr),
%% 	    %% QuerySignVal = lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= QuerySign]),

%% 	    io:format("DEBUG>>> billy_api_handler:create_payment  Signs=~p:~p~n", [QuerySignature, QuerySignVal]),

%% 	    %% Проверяем сигнатуру по закрытому ключу
%% 	    case QuerySignVal of
%% 		QuerySignature ->
%% 		    %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 		    %% Сформировать параметры создания транзакции

%% 		    %% Декодировать base64 параметры выбранного метода из запроса
%% 		    MethodParamsJson = base64:decode(SystemParamsBase64),
%% 		    MethodParams = jiffy:decode(MethodParamsJson),
%% 		    {MethodParamsProplist} = MethodParams,

%% 		    %% Декодировать base64 антифрод параметры из запроса
%% 		    AntiFraudParamsJson = base64:decode(AntiFraudParamsBase64),
%% 		    AntiFraudParams = jiffy:decode(AntiFraudParamsJson),

%% 		    %% Получить код валюты на основе 3х буквенного кода (iso4107)
%% 		    CcyNumber = billy_commons:get_currency_number(#{currency_alpha => Ccy}),

%% 		    %% Считаем сумму платежа в валюте обеспечения
%% 		    {ok, ErlJsonCcyRate} = billy_cbserver:get_rate(#{currency_alpha => Ccy, res_type => erljson}),
%% 		    {ok, ErlJsonPayeeCcyRate} = billy_cbserver:get_rate(#{currency_alpha => PayeeCcy, res_type => erljson}),

%% 		    {CcyRateProplist} = ErlJsonCcyRate,
%% 		    {PayeeCcyRateProplist} = ErlJsonPayeeCcyRate,

%% 		    CcyRateNominal = proplists:get_value(<<"nominal">>, CcyRateProplist),
%% 		    CcyRateValue = proplists:get_value(<<"value">>, CcyRateProplist),

%% 		    PayeeCcyRateNominal = proplists:get_value(<<"nominal">>, PayeeCcyRateProplist),
%% 		    PayeeCcyRateValue = proplists:get_value(<<"value">>, PayeeCcyRateProplist),

%% 		    RubCost = Amount * (CcyRateValue / CcyRateNominal),
%% 		    PayeeRubCost = Amount * (PayeeCcyRateValue / PayeeCcyRateNominal),
%% 		    FinRate = RubCost / PayeeRubCost,

%% 		    PayeeCost = round(Amount * FinRate),

%% 		    %% round((RubCost * PayeeCcyRateNominal) / PayeeCcyRateValue ),

%% 		    %% Формируем параметры транзакции
%% 		    FinTrParams = [
%% 				   {bill_id, BillId}, {method, Method}, {method_params, MethodParams}, {anti_fraud_params, AntiFraudParams},
%% 				   {rate, FinRate},
%% 				   {payee_ccy, PayeeCcy}, 
%% 				   {payee_cost, PayeeCost}
%% 				  ],

%% 		    TrParamsErlJson = {FinTrParams},
%% 		    TrParamsStr = jiffy:encode(TrParamsErlJson),

%% 		    io:format("DEBUG>>> billy_api_handler:create_payment  TrParamsStr=~p~n", [TrParamsStr]),

%% 		    TrCreateMap = #{
%% 		      type => 1, %% 1=сквозная транзакция
%% 		      params => TrParamsStr,
%% 		      user_id => MerchantId,

%% 		      currency_alpha => Ccy,
%% 		      currency_number => CcyNumber,

%% 		      cost => Amount,
%% 		      new_balance => 0,
%% 		      status => 0,
%% 		      create_date => calendar:local_time(),
%% 		      close_date => null
%% 		     },

%% 		    %% Создать транзакцию в cbserver
%% 		    case billy_cbserver:create_transaction(TrCreateMap) of
%% 			{ok, BillyTrId} ->
%% 			    %% Получить ссылку на оплату в сторонней системе
%% 			    RedirectUrl = case Method of

%% 					      <<"bpay">> ->

%% 						  GetPLinkParams = #{tr_id => BillyTrId, signature => Signature},
%% 						  billy_bpay_handler:get_payment_link(GetPLinkParams);

%% 					      <<"g2a">> ->
%% 						  GetPLinkParams = #{tr_id => BillyTrId,
%% 								     tr_cost => Amount, 
%% 								     tr_ccy => Ccy,
%% 								     signature => Signature},
%% 						  billy_g2a_handler:get_payment_link(GetPLinkParams);

%% 					      <<"robokassa">> ->

%% 						  Comment0 = http_uri:encode(io_lib:format("Заказ #~p (Игра ~ts)", [BillyTrId, MerchantName])),
%% 						  Comment = unicode:characters_to_binary(Comment0, utf8),

%% 						  GetPLinkParams = #{tr_id => BillyTrId,
%% 								     tr_cost => Amount, 
%% 								     tr_ccy => Ccy,
%% 								     comment => Comment
%% 								    },
%% 						  billy_robokassa_handler:get_payment_link(GetPLinkParams);

%% 					      <<"qiwi">> ->
%% 						  %% Читаем телефон из данных запроса
%% 						  %% !!! Это костыль, надо доделать проверку на чеке
%% 						  %% и читать данные из нормированых параметров
%% 						  Phone0 = proplists:get_value(<<"phone">>, MethodParamsProplist),
%% 						  Phone = io_lib:format("+~ts", [Phone0]),

%% 						  Comment0 = io_lib:format("Заказ #~p (Игра ~ts)", [BillyTrId, MerchantName]),
%% 						  Comment = unicode:characters_to_binary(Comment0, utf8),

%% 						  GetPLinkParams = #{tr_id => BillyTrId,
%% 								     tr_cost => Amount,
%% 								     tr_ccy => Ccy,
%% 								     phone => Phone0,
%% 								     comment => Comment,
%% 								     pay_source => "qw"
%% 								    },
%% 						  billy_qiwi_handler:get_payment_link(GetPLinkParams);

%% 					      <<"qiwicards">> ->
%% 						  %% Читаем телефон из данных запроса
%% 						  %% !!! Это костыль, надо доделать проверку на чеке
%% 						  %% и читать данные из нормированых параметров
%% 						  Phone0 = proplists:get_value(<<"phone">>, MethodParamsProplist),
%% 						  Phone = io_lib:format("+~ts", [Phone0]),

%% 						  Comment0 = io_lib:format("Заказ #~p (Игра ~ts)", [BillyTrId, MerchantName]),
%% 						  Comment = unicode:characters_to_binary(Comment0, utf8),

%% 						  GetPLinkParams = #{tr_id => BillyTrId,
%% 								     tr_cost => Amount,
%% 								     tr_ccy => Ccy,
%% 								     phone => Phone0,
%% 								     comment => Comment,
%% 								     pay_source => "card"
%% 								    },
%% 						  billy_qiwi_handler:get_payment_link(GetPLinkParams);

%% 					      <<"cloudpayments">> ->
%% 						  BaseLink = "https://somacase.com/payment/cloudpayments/checkout",
%% 						  %% BaseLink = "https://188.35.184.37:8008/payment/cloudpayments/checkout",
%% 						  A = unicode:characters_to_binary(io_lib:format("~ts?tr_id=~p&signature=~ts",
%% 												 [BaseLink, BillyTrId, Signature])),
%% 						  io:format("DEBUG>>> cloudpayments link >>>   ~ts~n", [A]),
%% 						  A;

%% 					      <<"connectum">> ->
%% 						  Email = proplists:get_value(<<"email">>, MethodParamsProplist),

%% 						  %% BaseLink = "http://127.0.0.1:8008/payment/connectum/create",
%% 						  BaseLink = "https://somacase.com/payment/connectum/create",
%% 						  A = unicode:characters_to_binary(io_lib:format("~ts?tr_id=~p&signature=~ts",
%% 												 [BaseLink, BillyTrId, Signature])),
%% 						  io:format("DEBUG>>> connectum link >>>   ~ts~n", [A]),
%% 						  A;

%% 					      <<"cauri">> ->
%% 						  Email = proplists:get_value(<<"email">>, MethodParamsProplist),

%% 						  %% BaseLink = "http://127.0.0.1:8008/payment/cauri/checkout",
%% 						  BaseLink = "https://somacase.com/payment/cauri/checkout",
%% 						  A = unicode:characters_to_binary(io_lib:format("~ts?tr_id=~p&signature=~ts",
%% 												 [BaseLink, BillyTrId, Signature])),
%% 						  io:format("DEBUG>>> cauri link >>>   ~ts~n", [A]),
%% 						  A;

%% 					      <<"payeer">> ->
%% 						  BaseLink = "https://bvvd.ru/payment/payeer/checkout",
%% 						  A = unicode:characters_to_binary(io_lib:format("~ts?tr_id=~p&signature=~ts",
%% 												 [BaseLink, BillyTrId, Signature])),
%% 						  io:format("DEBUG>>> payeer link >>>   ~ts~n", [A]),
%% 						  A;

%% 					      <<"bitpay">> ->

%% 						  NotificationUrl = "",
%% 						  %% RedirectUrl = "",

%% 						  GetPLinkParams = #{tr_id => BillyTrId,
%% 								     tr_cost => Amount,
%% 								     tr_ccy => Ccy
%% 								    },
%% 						  billy_bitpay_handler:get_payment_link(GetPLinkParams);

%% 					      <<"skinpay">> ->
%% 						  PartnerId = proplists:get_value(<<"partner_id">>, ReqParamsKV),
%% 						  Token = proplists:get_value(<<"token">>, ReqParamsKV),
%% 						  SecretKey = "cpk123oci21j389xmy128ny38127",
%% 						  SignatureSTR = lists:concat(io_lib:format("~ts~ts~ts~ts", [PartnerId, Token, Ccy, SecretKey])),

%% 						  io:format("DEBUG>>> skinpay SignatureSTR >>>   ~ts~n", [SignatureSTR]),

%% 						  MD5 = erlang:md5(SignatureSTR),
%% 						  SkinpaySign = lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= MD5]),

%% 						  BaseLink = "https://skinpay.gocs.pro/deposit",
%% 						  A = unicode:characters_to_binary(io_lib:format("~ts?partner_id=~ts&token=~ts&gw_transaction_id=~p&currency=~ts&sign=~ts&locale=ru",
%% 												 [BaseLink, PartnerId, Token, BillyTrId, Ccy, SkinpaySign])),
%% 						  io:format("DEBUG>>> skinpay link >>>   ~ts~n", [A]),
%% 						  A
%% 					  end,

%% 			    %% Вернуть результат
%% 			    io:format("DEBUG>>> billy_api_handler:create_payment  RedirectUrl=~p~n", [RedirectUrl]),

%% 			    Responce = {[ {status, ok}, {redirect_url, RedirectUrl} ]},
%% 			    Body = jiffy:encode(Responce),
%% 			    {ok, Body}
%% 		    end;
%% 		QuerySignVal ->
%% 		    %% Сигнатура неверна
%% 		    Responce = {[ {status, error}, {error_code, 3} ]},
%% 		    Body = jiffy:encode(Responce),
%% 		    {ok, Body}
%% 	    end
%% end


currency_rate(Req0) ->
    
    {ok, ReqParamsKV, _} = cowboy_req:read_urlencoded_body(Req0),

    %% Проверяем полученые данные
    PostData = [
		{ccy_in, proplists:get_value(<<"ccy_in">>, ReqParamsKV)},
		{ccy_out, proplists:get_value(<<"ccy_out">>, ReqParamsKV)}
	       ],
    
    %% Проверяем полученые данные
    case check_post_data(currency_rate, PostData) of

	{error, _} ->
	    %% Неверный формат входных данных
	    Responce = {[ {status, error}, {error_code, 1} ]},
	    Body = jiffy:encode(Responce),
	    {ok, Body};

	{ok, NormPostData} ->
	    CcyIn = proplists:get_value(ccy_in, NormPostData),
	    CcyOut = proplists:get_value(ccy_out, NormPostData),

	    QueryUrlTpl = "https://currency.paykassa.pro/?currency_in=~ts&currency_out=~ts",
	    QueryUrl = io_lib:format(QueryUrlTpl, [CcyIn, CcyOut]),
	    
	    Headers = [],
	    
	    QueryRes = case hackney:request(get, QueryUrl, Headers, <<>>, []) of
			   
			   %% Запрос выполнен успешно
			   {ok, 200, _RespHeaders, ClientRef}=HR ->
			       {ok, RespBody} = hackney:body(ClientRef),
			       DecodedRespBody = jiffy:decode(RespBody, [return_maps]),

			       RateData = maps:get(<<"data">>, DecodedRespBody),
			       RateVal = maps:get(<<"value">>, RateData),
			       
			       Responce = {[ {status, ok}, {rate, RateVal} ]},
			       Body = jiffy:encode(Responce),
			       {ok, Body};

			   %% Что-то не так
			   Resp -> 
			       error_logger:info_msg("DEBUG>>> billy_api_handler:currency_rate hackney Resp: ~p~n", [Resp]),
			       Responce = {[ {status, error}, {error_code, 500} ]},
			       Body = jiffy:encode(Responce),
			       {ok, Body}
		       end,
	    QueryRes	
    end.


unknown_method() ->
    io:format("DEBUG>>> billy_api_handler:unknown_method!~n"),
    Body = <<"Unknown method">>,
    {ok, Body}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Функции проверки данных POST запросов

check_post_data(create_payment, PostData) ->
    SAmount = proplists:get_value(amount, PostData),
    SCcy = proplists:get_value(ccy, PostData),
    SPayeeCcy = proplists:get_value(payee_ccy, PostData),
    SSystem = proplists:get_value(system, PostData),
    SSystemParams = proplists:get_value(system_params, PostData),

    SMerchantId = proplists:get_value(merchant_id, PostData),
    SBillId = proplists:get_value(bill_id, PostData),
    SDesc = proplists:get_value(desc, PostData),
    SAntiFraudParams = proplists:get_value(anti_fraud_params, PostData),
    SSignature = proplists:get_value(signature, PostData),
    
    CheckResult = [
		   {amount, billy_query_helper:check_integer(SAmount)},
		   {ccy, billy_query_helper:check_string(SCcy, 3)},
		   {payee_ccy, billy_query_helper:check_string(SPayeeCcy, 3)},
		   {system, billy_query_helper:check_string(SSystem, 20)},
		   {system_params, billy_query_helper:check_string(SSystemParams, 1024)},

		   {merchant_id, billy_query_helper:check_integer(SMerchantId)},
		   {bill_id, billy_query_helper:check_integer(SBillId)},
		   {desc, billy_query_helper:check_string(SDesc, 128)},
		   {anti_fraud_params, billy_query_helper:check_string(SAntiFraudParams, 1024)},
		   {signature, billy_query_helper:check_string(SSignature, 128)}
		  ],

    FinCheck = billy_query_helper:finaly_check(CheckResult),
    case FinCheck of
	true  -> {ok, CheckResult};
	false -> {error, CheckResult}
    end;

check_post_data(currency_rate, PostData) ->
    SCcyIn = proplists:get_value(ccy_in, PostData),
    SCcyOut = proplists:get_value(ccy_out, PostData),

    CheckResult = [
		   {ccy_in, billy_query_helper:check_string(SCcyIn, 3)},
		   {ccy_out, billy_query_helper:check_string(SCcyOut, 3)}
		  ],

    FinCheck = billy_query_helper:finaly_check(CheckResult),
    case FinCheck of
	true  -> {ok, CheckResult};
	false -> {error, CheckResult}
    end.


%% ======================================================================
%% STUFF FUNCTIONS
%% ======================================================================

get_payment_link(#{system := System, transaction_id := BillyTrId}) ->

    %% Читаем соответствующий php файл
    Filename = io_lib:format("priv/ephp/get_payment_link/~ts.php", [System]),
    case file:read_file(Filename) of
	{ok, PHP} ->

	    %% Создаём контекст
	    {ok, Ctx} = ephp:context_new(),
	    ephp:register_module(Ctx, billy_ephp_lib),

	    {ok, Output} = ephp_output:start_link(Ctx, false),
	    ephp_context:set_output_handler(Ctx, Output),

	    %% Создаём переменную с id транзакцией шлюза
	    ephp_context:set(Ctx, #variable{name = <<"billy_transaction_id">>}, BillyTrId),

	    %% Выполняем загруженный php код
	    try ephp:eval(Filename, Ctx, PHP) of
		{ok, _Return} ->
		    Result = ephp_context:get_output(Ctx),
		    ResultBin = unicode:characters_to_binary(Result, utf8),

		    io:format("DEBUG>>> api_handler:get_payment_link ResultBin=>~ts<",[ResultBin]),

		    ResultMap = jiffy:decode(ResultBin, [return_maps]),
		    case maps:get(<<"status">>, ResultMap) of
			<<"ok">> ->
			    {ok, maps:get(<<"payment_link">>, ResultMap)};
			<<"error">> ->
			    {error, maps:get(<<"message">>, ResultMap)}
		    end;
		{error, EvalErrReason, Index, File, Level, Data} = Error ->
		    ErrorMessage0 = ephp_context:get_output(Ctx),
		    ErrorMessage = unicode:characters_to_binary(ErrorMessage0, utf8),

		    %% lager:error("php eval error: ~p : ~ts : ~p ; Level=~p ; Data=~p", [EvalErrReason, File, Index, Level, Data]),
		    lager:error("php eval error: ~ts", [ErrorMessage]),

		    {error, eval_php_error, ErrorMessage}
	    catch
		TypeOfError:Exception ->
		    lager:error("php eval catch: ~p ; ex: ~p~n", [TypeOfError, Exception]),
		    {catch_error, TypeOfError, Exception}
	    end;
	{error, enoent} ->
	    lager:error("file not found: ~ts~n", [Filename]),
	    {error, enoent};
	{error, ReadErrReason} ->
	    lager:error("read file error: ~ts ; reason: ~ts~n", [Filename, ReadErrReason]),
	    {error, ReadErrReason}
    end.
