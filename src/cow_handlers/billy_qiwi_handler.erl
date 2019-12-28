-module(billy_qiwi_handler).

-export([init/2]).
-export([get_payment_link/1]).

-define(xml_prolog, "<?xml version=\"1.0\"?>").

init(Req, OptsMap) ->
    
    #{method := QueryMethod} = Req,
    #{method := ApiMethod} = OptsMap,

    ApiRes = case ApiMethod of
		 notification_p2p -> notification_p2p(Req);
		 notification_b2b -> notification_b2b(Req);
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



notification_p2p(Req) ->

    RespHeaders = #{<<"content-type">> => <<"application/json">>},

    %% Читаем и декодируем платёж
    {ok, ReqDataBin, _} = cowboy_req:read_body(Req),    
    ReqDataMap = jiffy:decode(ReqDataBin, [return_maps]),

    io:format("DEBUG>>> billy_qiwi_handler:notification_p2p ReqDataMap: ~p~n", [ReqDataMap]),

    QiwiBill = maps:get(<<"bill">>, ReqDataMap),
    
    %% Проверить статус оповещения
    QiwiBillStatusMap = maps:get(<<"status">>, QiwiBill),
    QiwiBillStatusVal = maps:get(<<"value">>, QiwiBillStatusMap),

    case QiwiBillStatusVal of
	<<"PAID">> ->

	    BillIdStr = maps:get(<<"billId">>, QiwiBill),
	    BillIdInt = billy_query_helper:check_integer(BillIdStr),

	    QiwiBillSiteId = maps:get(<<"siteId">>, QiwiBill),

	    QiwiBillAmountMap = maps:get(<<"amount">>, QiwiBill),	    
	    QiwiBillAmountStr = maps:get(<<"value">>, QiwiBillAmountMap),
	    QiwiBillAmountInt = round(billy_query_helper:check_float(QiwiBillAmountStr) * 100),
	    
	    QiwiBillCurrency = maps:get(<<"currency">>, QiwiBillAmountMap),

	    %% Получить транзакцию связанную с оплатой
	    {ok, [TrProplist | _]} = billy_transaction:get(#{transaction_id => BillIdInt}),
	    TrParamsStr = proplists:get_value(<<"params">>, TrProplist),
	    TrParamsMap = jiffy:decode(TrParamsStr, [return_maps]),

	    %% Загружаем конфиг мерчанта по платёжной системе
	    MerchantId = proplists:get_value(<<"merchant_id">>, TrProplist),
	    PaySystemKey = maps:get(<<"system">>, TrParamsMap),
	    {ok, PaysystemConfigMap} = billy_config:get(#{merchant_id => MerchantId, paysystem_key => PaySystemKey}), 

	    QiwiApiSecretKey = maps:get(<<"qiwi_api_secret_key">>, PaysystemConfigMap),

	    %% Проверить сигнатуру
	    QiwiSignParams = #{
	      amount => QiwiBillAmountStr,
	      ccy => QiwiBillCurrency,
	      bill_id => BillIdStr,
	      site_id => QiwiBillSiteId,
	      status => QiwiBillStatusVal
	     },

	    %% Прочитать HTTP заголовок X-Api-Signature
	    QiwiSign = cowboy_req:header(<<"x-api-signature-sha256">>, Req),
	    case check_qiwi_signature_p2p(QiwiSignParams, QiwiSign, QiwiApiSecretKey) of
		true ->
		    TrCost = proplists:get_value(<<"amount">>, TrProplist),
		    TrCurrency = proplists:get_value(<<"ccy_alpha">>, TrProplist),

		    %% Проверить сумму транзакции
		    case {QiwiBillAmountInt, QiwiBillCurrency} of
			{TrCost, TrCurrency} ->

			    ProcessResult = <<"success">>,

			    case billy_payment:process_transaction(#{transaction_id => BillIdInt, process_result => ProcessResult}) of
				ok ->
				    {output, jiffy:encode(#{error => 0}), RespHeaders};

				{error, transaction_already_processed} ->
				    {output, jiffy:encode(#{error => 0}), RespHeaders};

				{error, Reason} ->
				    {output, jiffy:encode(#{error => 0}), RespHeaders}
			    end;
			{TrCost, _} -> 
			    {output, jiffy:encode(#{error => 0}), RespHeaders};
			{_, TrCurrency} -> 
			    {output, jiffy:encode(#{error => 0}), RespHeaders};
			_ ->
			    {output, jiffy:encode(#{error => 0}), RespHeaders}
		    end;
		_ ->
		    io:format("DEBUG>>> billy_qiwi_handler:notification_p2p check_qiwi_signature ERROR!~n"),

		    {output, jiffy:encode(#{error => 0}), RespHeaders}
	    end;
	_ ->
	    {output, jiffy:encode(#{error => 0}), RespHeaders}
    end.


notification_b2b(Req) ->
    io:format("DEBUG>>> billy_qiwi_handler:notification! Req=~p~n", [Req]),

    %% Прочитать параметры и авторизовать платёж
    {ok, ReqParamsKV, _} = cowboy_req:read_urlencoded_body(Req),
    io:format("DEBUG>>> billy_qiwi_handler:notification  ReqParamsKV=~p~n", [ReqParamsKV]),

    %% Проверить параметры на валидность
    PostData = [
		%% 
		{command, proplists:get_value(<<"command">>, ReqParamsKV)},
		
		%% уникальный идентификатор счета в системе провайдера (любая непустая строка до 200 символов)
		{bill_id, proplists:get_value(<<"bill_id">>, ReqParamsKV)},
		
		%% (string) Статус счета
		{status, proplists:get_value(<<"status">>, ReqParamsKV)},
		
		%% (int) Код ошибки
		{error, proplists:get_value(<<"error">>, ReqParamsKV)},
		
		%% (float | xx.xx) Сумма счета. Способ округления зависит от валюты.
		{amount, proplists:get_value(<<"amount">>, ReqParamsKV)},
		
		%% (string) Идентификатор учетной записипользователя, которому выставляется счет. Равен номеру телефона пользователя с префиксом "tel:"
		{user, proplists:get_value(<<"user">>, ReqParamsKV)},
		
		%% (string) Имя провайдера (типа наше имя, передаём при создании счёта на оплату)
		{prv_name, proplists:get_value(<<"prv_name">>, ReqParamsKV)},
		
		%% (string) Код валюты пополнения. 3х буквенный iso4217
		{ccy, proplists:get_value(<<"ccy">>, ReqParamsKV)},
		
		%% (string) Комментарий
		{comment, proplists:get_value(<<"comment">>, ReqParamsKV)}
	       ],
    
    %% Проверить параметры на валидность
    case check_post_data(notification, PostData) of
	{error, CheckRes} ->
	    io:format("DEBUG>>> billy_qiwi_handler:notification CheckRes: ~p~n", [CheckRes]),
	    Responce = {[ {status, error}, {message, <<"Query params error!">>} ]},
	    Body = jiffy:encode(Responce),
	    {output, Body};
	{ok, NormPostData} ->
	    io:format("DEBUG>>> billy_qiwi_handler:notification NormPostData: ~p~n", [NormPostData]),
	    %% Проверить статус оповещения
	    Status = proplists:get_value(status, NormPostData),
	    
	    case Status of
		<<"paid">> ->
		    %% Проверить сигнатуру
		    QiwiSignParams = [
				      {command, proplists:get_value(command, NormPostData)},
				      {bill_id, proplists:get_value(bill_id, NormPostData)},
				      {status, proplists:get_value(status, NormPostData)},
				      {error, proplists:get_value(error, NormPostData)},
				      {amount, proplists:get_value(amount, NormPostData)},
				      {user, proplists:get_value(user, NormPostData)},
				      {prv_name, proplists:get_value(prv_name, NormPostData)},
				      {ccy, proplists:get_value(ccy, NormPostData)},
				      {comment, proplists:get_value(comment, NormPostData)}
				     ],

		    %% Прочитать HTTP заголовок X-Api-Signature
		    QiwiSign = cowboy_req:header(<<"x-api-signature">>, Req),

		    case check_qiwi_signature(QiwiSignParams, QiwiSign) of
			true ->
			    BillId = proplists:get_value(bill_id, NormPostData),
			    
			    io:format("DEBUG>>> billy_qiwi_handler:notification check_qiwi_signature OK~n"),

			    %% Получить транзакцию связанную с оплатой
			    case billy_cbserver:get_transaction(#{transaction_id => BillId, res_type => json}) of
				{ok, TrProplist} ->
				    QiwiBillAmount = round(
						       billy_query_helper:check_float(
							 proplists:get_value(amount, NormPostData)) * 100),
				    
				    QiwiBillCurrency = proplists:get_value(ccy, NormPostData),
				    
				    TrCost = proplists:get_value(<<"cost">>, TrProplist),
				    TrCurrency = proplists:get_value(<<"currency_alpha">>, TrProplist),
				    
				    %% Проверить сумму транзакции
				    case {QiwiBillAmount, QiwiBillCurrency} of
					{TrCost, TrCurrency} ->
					    %% Обработать транзакцию. т.е.
					    %% 1) Обновить статус по базе что транзакция принята
					    %% 2) Обновить баланс мерчанта
					    %% TrType = proplists:get_value(<<"type">>, TrProplist),
					    
					    ProcessResult = <<"success">>,
					    
					    case billy_payment:process_transaction(#{transaction_id => BillId, process_result => ProcessResult}) of
						{ok, _NewBalance} ->

						    %% Немедленно вызвать IPN к мерчанту
						    NotifyParamsMap = #{transaction_id => BillId},
						    wpool:cast(billy_ipn_wpool, {notify, NotifyParamsMap}),
						    
						    %% Вернуть результат
						    {output, make_qiwi_xml("0"), #{<<"content-type">> => <<"text/xml">>}};
						{error, transaction_already_processed} ->
						    XmlText = make_qiwi_xml("0"),
						    io:format("DEBUG>>> billy_qiwi_handler:notification XmlText:~n~p~n", [XmlText]),
						    {output, XmlText, #{<<"content-type">> => <<"text/xml">>}};
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
			_ ->
			    {output, <<"SIGNATURE ERROR">>}
		    end;
		_ ->
		    {output, make_qiwi_xml("0"), #{<<"content-type">> => <<"text/xml">>}}
	    end
    end.


success(Req) ->
    %% Прочитать параметр order
    #{order := OrderId} = cowboy_req:match_qs([order], Req),

    %% получить транзакцию связанную с оплатой
    case billy_transaction:get(#{transaction_id => OrderId}) of
	{ok, [TrProplist | _]} ->

	    MerchantId = proplists:get_value(<<"merchant_id">>, TrProplist),

	    %% Загружаем конфиг мерчанта
	    {ok, MerchantConfig} = billy_config:get(#{merchant_id => MerchantId}),
	    
	    %% Получаем URL перенаправления для успешного платежа
	    {ok, [{_, MerchSuccessUrl}]} = billy_config:get(#{merchant_config => MerchantConfig, key => "success_url"}),

	    {redirect, MerchSuccessUrl}
    end.


fail(Req) ->
    %% Прочитать параметр order
    #{order := OrderId} = cowboy_req:match_qs([order], Req),

    %% получить транзакцию связанную с оплатой
    case billy_transaction:get(#{transaction_id => OrderId}) of
	{ok, [TrProplist | _]} ->

	    MerchantId = proplists:get_value(<<"merchant_id">>, TrProplist),

	    %% Загружаем конфиг мерчанта
	    {ok, MerchantConfig} = billy_config:get(#{merchant_id => MerchantId}),
	    
	    %% Получаем URL перенаправления для отклонённого платежа
	    {ok, [{_, MerchFailUrl}]} = billy_config:get(#{merchant_config => MerchantConfig, key => "fail_url"}),

	    {redirect, MerchFailUrl}
    end.




unknown_method() ->
    io:format("DEBUG>>> billy_qiwi_handler:unknown_method!~n"),
    Body = <<"Unknown method">>,
    {output, Body}.



get_payment_link(#{tr_id := TrId, tr_cost := TrCost, tr_ccy := TrCurrency, phone := Phone, comment := Comment, pay_source := PaySource}) ->
    %% Форматируем сумму счёта
    FormatedTrCost = list_to_binary(float_to_list(TrCost / 100, [{decimals, 2}])),

    %% Формируем авторизационный заголовок
    AuthorizationHeaderData = list_to_binary(io_lib:format("Bearer ~ts", [billy_config:get(qiwi_api_secret_key)])),

    %% Формируем ссылку API Qiwi 
    QueryUrlTpl = "https://api.qiwi.com/partner/bill/v1/bills/~p",
    QueryUrl = list_to_binary(io_lib:format(QueryUrlTpl, [TrId])),

    %% Считаем дату завершения выставленного счёта
    {MegaS, S, MicroS} = erlang:timestamp(),
    S2 = S + 172800, %% +2 days
    Lifetime8601 = iso8601:format({MegaS, S2, MicroS}),
    Lifetime0 = re:replace(Lifetime8601, "Z", "",[global, {return, binary}]), %% QIWI need time without 'Z'
    Lifetime = list_to_binary(io_lib:format("~ts+03:00", [Lifetime0])),

    %% Формируем параметры запроса
    QueryBodyMap = #{
      <<"amount">> => #{
	<<"currency">> => TrCurrency,
	<<"value">> => FormatedTrCost
       },
      <<"comment">> => Comment,
      <<"expirationDateTime">> => Lifetime,
      <<"customer">> => #{
	<<"phone">> => Phone
       }
     },
    QueryBody = jiffy:encode(QueryBodyMap),

    %% Вызываем ссылку API Qiwi и получаем параметры выставленного счёта
    Headers = [
	       {<<"Authorization">>, AuthorizationHeaderData},
	       {<<"Content-Type">>, <<"application/json">>},
	       {<<"Accept">>, <<"application/json">>}
	      ],
    Options = [],
    case hackney:request(put, QueryUrl, Headers, QueryBody, Options) of
	%% Запрос выполнен успешно
	{ok, 200, RespHeaders, ClientRef}=HR ->
	    %% Получить тело ответа и определить что сервер мерчанта закрыл транзакцию
	    {ok, RespBody} = hackney:body(ClientRef),
	    
	    io:format("DEBUG>>> billy_qiwi_handler:get_payment_link HackeyReq:=~p~n", [HR]),
	    io:format("DEBUG>>> billy_qiwi_handler:get_payment_link RespBody:=~p~n", [RespBody]),
	    
	    DecodedRespBody = jiffy:decode(RespBody, [return_maps]),

	    io:format("DEBUG>>> billy_qiwi_handler:get_payment_link Query DecodedRespBody = ~p~n", [DecodedRespBody]),
	    
	    PayUrl = maps:get(<<"payUrl">>, DecodedRespBody),

	    SuccessUri = http_uri:encode(io_lib:format("https://gaminatorx.ru/payment/qiwi/success?order=~p", [TrId])),

	    list_to_binary(io_lib:format("~ts&paySource=~ts&successUrl=~ts", [PayUrl, PaySource, SuccessUri]));

	{ok, 400, RespHeaders, ClientRef}=HR ->

	    %% Получить тело ответа и определить что сервер мерчанта закрыл транзакцию
	    {ok, RespBody} = hackney:body(ClientRef),
	    
	    io:format("DEBUG>>> billy_qiwi_handler:get_payment_link HackeyReq:=~p~n", [HR]),
	    io:format("DEBUG>>> billy_qiwi_handler:get_payment_link RespBody:=~p~n", [RespBody]),
	    <<"/">>;

	{ok, 401, RespHeaders, ClientRef}=HR ->

	    {ok, RespBody} = hackney:body(ClientRef),
	    
	    io:format("DEBUG>>> billy_qiwi_handler:get_payment_link HackeyReq:=~p~n", [HR]),
	    io:format("DEBUG>>> billy_qiwi_handler:get_payment_link RespBody:=~p~n", [RespBody]),
	    <<"/">>;

	{ok, 415, RespHeaders, ClientRef}=HR ->

	    {ok, RespBody} = hackney:body(ClientRef),
	    
	    io:format("DEBUG>>> billy_qiwi_handler:get_payment_link HackeyReq:=~p~n", [HR]),
	    io:format("DEBUG>>> billy_qiwi_handler:get_payment_link RespBody:=~p~n", [RespBody]),
	    <<"/">>;

	Resp -> 
	    io:format("DEBUG>>> billy_qiwi_handler:get_payment_link ibrowse Resp: ~p~n", [Resp]),
	    <<"/">>
    end.


%% Формируем ссылку на оплату в системе QIWI
%% get_payment_link_b2b(TransactionId, TrCost, TrCurrency, UserTel, PaySource) ->
%% get_payment_link_b2b(#{tr_id := TrId, tr_cost := TrCost, tr_ccy := TrCurrency, phone := Phone, comment := Comment, pay_source := PaySource}) ->
%%     %% Форматируем сумму счёта
%%     FormatedTrCost = float_to_list(TrCost / 100, [{decimals, 2}]),

%%     %% Формируем авторизационный заголовок
%%     AuthorizationStr = io_lib:format("~ts:~ts", [billy_config:get(qiwi_api_id), billy_config:get(qiwi_api_password)]),
%%     AuthorizationBase64 = base64:encode(list_to_binary(AuthorizationStr)),
%%     AuthorizationHeaderData = io_lib:format("Basic ~ts", [AuthorizationBase64]),

%%     io:format("DEBUG>>> billy_qiwi_handler:generate_payment_link AuthorizationStr = ~ts~n", [AuthorizationStr]),
%%     io:format("DEBUG>>> billy_qiwi_handler:generate_payment_link AuthorizationHeaderData = ~ts~n", [AuthorizationHeaderData]),

%%     %% Выставить счёт пользователю
%%     QueryUrlTpl = "https://api.qiwi.com/api/v2/prv/~ts/bills/~p",
%%     QueryUrl = list_to_binary(io_lib:format(QueryUrlTpl, [billy_config:get(qiwi_prv_id), TrId])),

%%     %% Comment = io_lib:format("Order number #~p", [TrId]),

%%     {MegaS, S, MicroS} = erlang:timestamp(),
%%     S2 = S + 172800, %% +2 days
%%     Lifetime8601 = iso8601:format({MegaS, S2, MicroS}),
%%     Lifetime = re:replace(Lifetime8601, "Z", "",[global, {return, binary}]), %% QIWI need time without 'Z'

%%     QueryBody0 = io_lib:format("user=tel:~ts&amount=~ts&ccy=~ts&comment=~ts&lifetime=~ts",
%% 			      [http_uri:encode(Phone), FormatedTrCost, TrCurrency, Comment, Lifetime]),
    
%%     QueryBody = unicode:characters_to_binary(QueryBody0, utf8),

%%     io:format("DEBUG>>> billy_qiwi_handler:get_payment_link QueryUrl:=~p~n", [QueryUrl]),
%%     io:format("DEBUG>>> billy_qiwi_handler:get_payment_link QueryBody=~ts~n", [QueryBody]),

%%     %% Вызываем API полатёжного шлюза qiwi и получаем параметры выставленного счёта
%%     Headers = [
%% 	       {<<"Authorization">>, list_to_binary(AuthorizationHeaderData)},
%% 	       {<<"Content-Type">>, <<"application/x-www-form-urlencoded; charset=utf-8">>},
%% 	       {<<"Accept">>, <<"text/json">>}
%% 	      ],
%%     Options = [],
%%     case hackney:request(put, QueryUrl, Headers, QueryBody, Options) of
%% 	%% Запрос выполнен успешно
%% 	{ok, 200, RespHeaders, ClientRef}=HR ->
%% 	    %% Получить тело ответа и определить что сервер мерчанта закрыл транзакцию
%% 	    {ok, RespBody} = hackney:body(ClientRef),
	    
%% 	    io:format("DEBUG>>> billy_qiwi_handler:get_payment_link HackeyReq:=~p~n", [HR]),
%% 	    io:format("DEBUG>>> billy_qiwi_handler:get_payment_link RespBody:=~p~n", [RespBody]),
	    
%% 	    DecodedRespBody = jiffy:decode(RespBody, [return_maps]),

%% 	    io:format("DEBUG>>> billy_qiwi_handler:get_payment_link Query DecodedRespBody = ~p~n", [DecodedRespBody]),
	    
%% 	    DecodedResp = maps:get(<<"response">>, DecodedRespBody),
%% 	    Bill = maps:get(<<"bill">>, DecodedResp),
%% 	    BillId = maps:get(<<"bill_id">>, Bill),

%% 	    SuccessUri = http_uri:encode(io_lib:format("https://gaminatorx.ru/payment/qiwi/success", [])),
%% 	    FailUri = http_uri:encode(io_lib:format("https://gaminatorx.ru/payment/qiwi/fail", [])),

%% 	    list_to_binary(io_lib:format("https://bill.qiwi.com/order/external/main.action?shop=~ts&transaction=~ts&billref=~ts&iframe=true&successUrl=~ts&failUrl=~ts", [billy_config:get(qiwi_prv_id), BillId, PaySource, SuccessUri, FailUri]));
%% 	{ok, 400, RespHeaders, ClientRef}=HR ->
%% 	    %% Получить тело ответа и определить что сервер мерчанта закрыл транзакцию
%% 	    {ok, RespBody} = hackney:body(ClientRef),
	    
%% 	    io:format("DEBUG>>> billy_qiwi_handler:get_payment_link HackeyReq:=~p~n", [HR]),
%% 	    io:format("DEBUG>>> billy_qiwi_handler:get_payment_link RespBody:=~p~n", [RespBody]),
%% 	    <<"/">>;
%% 	{ok, 401, RespHeaders, ClientRef}=HR ->
%% 	    {ok, RespBody} = hackney:body(ClientRef),
	    
%% 	    io:format("DEBUG>>> billy_qiwi_handler:get_payment_link HackeyReq:=~p~n", [HR]),
%% 	    io:format("DEBUG>>> billy_qiwi_handler:get_payment_link RespBody:=~p~n", [RespBody]),
%% 	    <<"/">>;
%% 	Resp -> 
%% 	    io:format("DEBUG>>> billy_qiwi_handler:get_payment_link ibrowse Resp: ~p~n", [Resp]),
%% 	    <<"/">>
%%     end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Функции проверки данных POST запросов

check_post_data(notification_b2b, PostData) ->
    Command = proplists:get_value(command, PostData),
    BillId = proplists:get_value(bill_id, PostData),
    Status = proplists:get_value(status, PostData),
    Error = proplists:get_value(error, PostData),
    Amount = proplists:get_value(amount, PostData),
    User = proplists:get_value(user, PostData),
    PrvName = proplists:get_value(prv_name, PostData),
    Ccy = proplists:get_value(ccy, PostData),
    Comment = proplists:get_value(comment, PostData),
    
    CheckResult = [
 		   {command, billy_query_helper:check_binary_string(Command)},
 		   {bill_id, billy_query_helper:check_integer(BillId)},
 		   {status, billy_query_helper:check_binary_string(Status)},
 		   {error, billy_query_helper:check_binary_string(Error)},
 		   {amount, billy_query_helper:check_binary_string(Amount)},
 		   {user, billy_query_helper:check_binary_string(User)},
 		   {prv_name, billy_query_helper:check_binary_string(PrvName)},
 		   {ccy, billy_query_helper:check_binary_string(Ccy)},
 		   {comment, billy_query_helper:check_binary_string(Comment)}
 		  ],
    FinCheck = billy_query_helper:finaly_check(CheckResult),
    
    case FinCheck of
	true  -> {ok, CheckResult};
	false -> {error, CheckResult}
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Служебные функции


generate_qiwi_b2b_signature(Params) ->
    SortedParams = lists:sort(fun(A, B) -> 
				      {Key1, _Val1} = A,
				      {Key2, _Val2} = B,
				      Key1 < Key2
			      end, Params),
    io:format("DEBUG>>> billy_qiwi_handler:generate_qiwi_signature sorted params: ~p~n", [SortedParams]),
    SignatureStrHead0 = lists:foldl(fun(P, Str) -> 
					    case P of
						{_, "undefined"} -> Str;
						{salt, _} -> Str;
						{_Key, Value} ->
						    ValueChk = case Value of
								   Value when is_integer(Value) ->
								       unicode:characters_to_binary(
									 lists:flatten(io_lib:format("~p", [Value])), utf8
									);
								   Value when is_float(Value) ->
								       unicode:characters_to_binary(
									 lists:flatten(io_lib:format("~p", [Value])), utf8
									);
								   Value when is_list(Value) ->
								       unicode:characters_to_binary(Value, utf8);
								   Value when is_binary(Value) ->
								       Value
							       end,
						    PipeSymbol = <<"|">>,
						    <<Str/binary, ValueChk/binary, PipeSymbol/binary>>
						    %% B = lists:flatten(io_lib:format("~ts|", [ValueChk])),
							%% lists:concat([Str, B])
					    end
				    end, <<"">>, SortedParams),
    
    %% Убрать последний символ '|' в конце строки параметров
    SA = size(SignatureStrHead0)-1,
    <<SignatureStr:SA/binary, _>> = SignatureStrHead0,
    
    ShaKey = unicode:characters_to_binary(billy_config:get(qiwi_api_notification_password), utf8),
    ShaData = SignatureStr,

    SignatureValue = base64:encode(crypto:hmac(sha, ShaKey, ShaData)),
    io:format("DEBUG>>> billy_qiwi_handler:generate_qiwi_signature value: ~ts~n", [SignatureValue]),
    SignatureValue.



check_qiwi_signature(QiwiSignParams, QiwiSign) when is_list(QiwiSign) ->
    MyQiwiSign = generate_qiwi_b2b_signature(QiwiSignParams),
    QiwiSignBinary = list_to_binary(QiwiSign),
    case MyQiwiSign of
	QiwiSignBinary -> true;
	_ -> false
    end;
check_qiwi_signature(QiwiSignParams, QiwiSign) when is_binary(QiwiSign) ->
    MyQiwiSign = generate_qiwi_b2b_signature(QiwiSignParams),
    case MyQiwiSign of
	QiwiSign -> true;
	_ -> false
    end.



check_qiwi_signature_p2p(QiwiSignParams, QiwiSign, QiwiApiSecretKey) ->

    #{
       amount := Amount,
       ccy := Currency,
       bill_id := BillId,
       site_id := SiteId,
       status := Status
     } = QiwiSignParams,

    ShaKey = QiwiApiSecretKey,
    ShaData = list_to_binary(io_lib:format("~ts|~ts|~ts|~ts|~ts", [Currency, Amount, BillId, SiteId, Status])),
    SignatureDigest = crypto:hmac(sha256, ShaKey, ShaData),
    SignatureStr = list_to_binary(lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= SignatureDigest])),

    case SignatureStr of
	QiwiSign -> true;
	_ -> false
    end.


make_qiwi_xml(ResultCode) ->
    XmlFields = {result, [{result_code, [ResultCode]}]},
    make_doc_xml(XmlFields).

make_doc_xml(XmlFields) ->
    Xml = xmerl:export_simple([XmlFields], xmerl_xml, [{prolog, ?xml_prolog}]),
    unicode:characters_to_binary(Xml).


