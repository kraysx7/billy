-module(billy_bitpay_handler).

-export([init/2]).
-export([get_payment_link/1]).

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

%% Функция генерирует ссылку для оплаты заказа в системе bitpay
get_payment_link(#{tr_id := BillyTrId,
		   tr_cost := Amount,
		   tr_ccy := Ccy}) ->

    FilePath = filename:absname("../priv/bitpay_nodejs/node_modules/bitpay-rest/examples/create-invoice.js"),

    FormatedAmount = list_to_binary(float_to_list(Amount / 100, [{decimals, 2}])),

    NotificationUrl = "",
    RedirectUrl = "",

    Command = io_lib:format("node ~ts ~ts ~ts ~p", [
						   FilePath,
						   FormatedAmount,
						   Ccy,
						   BillyTrId
						  ]),
    RawResponse = os:cmd(Command),

    %% io:format("RawResponse >>>~ts<<<~n", [unicode:characters_to_binary(RawResponse, utf8)]),

    Invoice = jiffy:decode(unicode:characters_to_binary(RawResponse, utf8)),

    {InvoiceList} = Invoice,
    BitpayLink = proplists:get_value(<<"url">>, InvoiceList),

    % Make signature
    InvoiceTime = proplists:get_value(<<"invoiceTime">>, InvoiceList),
    Id = binary_to_list(proplists:get_value(<<"id">>, InvoiceList)),
    Sign = list_to_binary(billy_commons:hex_md5(lists:concat(io_lib:format("~p~ts", [InvoiceTime, Id])))),

    {ok, Transaction} = billy_cbserver:get_transaction(#{transaction_id => BillyTrId, res_type => json}),

    TrParamsBin = proplists:get_value(<<"params">>, Transaction),
    TrParams = jiffy:decode(TrParamsBin, [return_maps]),
    NewTrParams = maps:merge(TrParams, #{sign => Sign}),
    NewTrParamsBin = jiffy:encode(NewTrParams),

    billy_cbserver:update_transaction(#{transaction_id => BillyTrId, params => NewTrParamsBin, params_type => json_str}),

    BitpayLink.



notification(Req) ->
    %% Прочитать параметры и авторизовать платёж
    {ok, ReqBody, _} = cowboy_req:read_body(Req),
    {ReqParamsKV} = jiffy:decode(ReqBody),
    PosData0 = proplists:get_value(<<"posData">>, ReqParamsKV, {[]}),
    case PosData0 of 
	{[]} ->  {output, []};
	<<"[object Object]">> -> {output, []};
	PosData0 ->
	    {PosData} =  jiffy:decode(PosData0),
	    %% Проверить параметры на валидность
	    PostData = [
			{id, proplists:get_value(<<"id">>, ReqParamsKV)},
			{url, proplists:get_value(<<"url">>, ReqParamsKV)},
			{ref, proplists:get_value(<<"ref">>, PosData, <<"0">>)},
			{status, proplists:get_value(<<"status">>, ReqParamsKV)},
			{price, proplists:get_value(<<"price">>, ReqParamsKV)},
			{currency, proplists:get_value(<<"currency">>, ReqParamsKV)},
			{invoiceTime, proplists:get_value(<<"invoiceTime">>, ReqParamsKV)},
			{expirationTime, proplists:get_value(<<"expirationTime">>, ReqParamsKV)},
			{currentTime, proplists:get_value(<<"currentTime">>, ReqParamsKV)}
		       ],

	    %% Проверить параметры на валидность
	    case check_post_data(notification, PostData) of
		{error, CheckRes} ->
		    Responce = {[ {error, {[ {message, "Query params error!"} ]}} ]},
		    {output, jiffy:encode(Responce)};
		{ok, NormPostData} ->
		    TrId = proplists:get_value(ref, NormPostData),
		    case TrId of
			0 -> {output, []};
			_ ->
			    case billy_cbserver:get_transaction(#{transaction_id => TrId, res_type => json}) of
				{ok, TrProplist} ->
				    BitPayId = proplists:get_value(id, NormPostData),
				    InvoiceTime = proplists:get_value(invoiceTime, NormPostData),

				    BitPayCost = proplists:get_value(price, NormPostData)*100,
				    BitPayCcy = proplists:get_value(currency, NormPostData),

				    TrType = proplists:get_value(<<"type">>, TrProplist),
				    TrCost = proplists:get_value(<<"cost">>, TrProplist),
				    TrCurrency = proplists:get_value(<<"currency_alpha">>, TrProplist),

				    %% Сохранить токен в параметрах транзакции
				    TrParamsBin = proplists:get_value(<<"params">>, TrProplist),
				    TrParams = jiffy:decode(TrParamsBin, [return_maps]),

				    SignatureValue = maps:get(<<"sign">>, TrParams, <<"">>),

				    %% Проверить сумму и валюту транзакции
				    case {BitPayCost, BitPayCcy} of
					{TrCost, TrCurrency} ->
					    case check_signature(InvoiceTime, BitPayId, SignatureValue) of
						ok ->
						    ProcessResult = <<"success">>,
						    case billy_payment:process_transaction(#{transaction_id => TrId, process_result => ProcessResult}) of
							{ok, _NewBalance} ->
							    {output, []};
							{error, transaction_already_processed} ->
							    {output, []};
							    %% Responce = {[ {error, {[ {message, "Transaction already processed!"} ]}} ]},
							    %% {output, jiffy:encode(Responce)};
							{error, Reason} ->
							    Responce = {[ {error, {[ {message, "Payment process error!"} ]}} ]},
							    {output, jiffy:encode(Responce)}
						    end;
						_ ->
						    Responce = {[ {error, {[ {message, "Query signature error!"} ]}} ]},
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
				    end;
				_ ->
				    Responce = {[ {error,  {[ {message, "Can't get transaction from cbserver!"} ]}} ]},
				    {output, jiffy:encode(Responce)}
			    end
		    end
	    end	    
    end.


unknown_method() ->
    io:format("DEBUG>>> billy_bitpay_handler:unknown_method!~n"),
    Body = <<"Unknown method">>,
    {ok, Body}.


%% %% Проверить параметры на валидность
%% PostData = [
%% 		{id, proplists:get_value(<<"id">>, ReqParamsKV)},
%% 		{url, proplists:get_value(<<"url">>, ReqParamsKV)},
%% 		{ref, proplists:get_value(<<"ref">>, PosData)},
%% 		{status, proplists:get_value(<<"status">>, ReqParamsKV)},
%% 		{btcPrice, proplists:get_value(<<"btcPrice">>, ReqParamsKV)},
%% 		{price, proplists:get_value(<<"price">>, ReqParamsKV)},
%% 		{currency, proplists:get_value(<<"currency">>, ReqParamsKV)},
%% 		{invoiceTime, proplists:get_value(<<"invoiceTime">>, ReqParamsKV)},
%% 		{expirationTime, proplists:get_value(<<"expirationTime">>, ReqParamsKV)},
%% 		{currentTime, proplists:get_value(<<"currentTime">>, ReqParamsKV)}
%% 	       ],


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Функции проверки данных POST запросов

check_post_data(notification, PostData) ->
    SId = proplists:get_value(id, PostData),
    SUrl = proplists:get_value(url, PostData),
    SRef = proplists:get_value(ref, PostData),
    SStatus = proplists:get_value(status, PostData),
    SPrice = proplists:get_value(price, PostData),
    SCurrency = proplists:get_value(currency, PostData),
    SInvoiceTime = proplists:get_value(invoiceTime, PostData),
    SExpirationTime = proplists:get_value(expirationTime, PostData),
    SCurrentTime = proplists:get_value(currentTime, PostData),
    CheckResult = [
    		   {id, list_to_binary(billy_query_helper:check_string(SId, 32))},
    		   {url, list_to_binary(billy_query_helper:check_string(SUrl, 128))},
    		   {ref, billy_query_helper:check_integer(SRef)},
    		   {status, list_to_binary(billy_query_helper:check_string(SStatus))},
    		   {price, billy_query_helper:check_integer(SPrice)},
    		   {currency, list_to_binary(billy_query_helper:check_string(SCurrency))},
    		   {invoiceTime, billy_query_helper:check_integer(SInvoiceTime)},
    		   {expirationTime, billy_query_helper:check_integer(SExpirationTime)},
    		   {currentTime, billy_query_helper:check_integer(SCurrentTime)}
    		  ],
    FinCheck = billy_query_helper:finaly_check(CheckResult),
    case FinCheck of
    	true  -> {ok, CheckResult};
    	false -> {error, CheckResult}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Другие функции

check_signature([],_,_) -> badarg;
check_signature(_,[],_) -> badarg;
check_signature(_,_,[]) -> badarg;
check_signature(InvoiceTime,
		Id,
		SignatureValue) ->
    MySignatureValue = list_to_binary(billy_commons:hex_md5(lists:concat(io_lib:format("~p~ts", [InvoiceTime, Id])))),
    case MySignatureValue of
	SignatureValue -> ok;
	_ -> signature_error
    end;
check_signature(_,_,_) -> badarg.

