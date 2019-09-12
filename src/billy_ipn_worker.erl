-module(billy_ipn_worker).

-behaviour(gen_server).
%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Args) ->
    io:format("DEBUG>>> billy_ipn_worker(~p):init Args=~p~n", [self(),Args]),
    {ok, #state{}}.

handle_call(Command, _From, State) ->
    io:format("DEBUG>>> billy_ipn_worker(~p):handle_call(~p, ~p)~n", [self(), Command, State]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast({notify, #{transaction_id := TrId}} = Command, State) ->
    %% io:format("DEBUG>>> billy_ipn_worker(~p):handle_cast(~p, ~p)~n", [self(), Command, State]),

    %% Получить транзакцию из базы и проверить статус
    {ok, TrData} = billy_cbserver:get_transaction(#{transaction_id => TrId, res_type => json}),
    TrStatus = proplists:get_value(<<"status">>, TrData),
    MerchantId = proplists:get_value(<<"user_id">>, TrData),

    %% io:format("DEBUG>>> billy_ipn_worker(~p):handle_cast TrData=~p~n", [self(), TrData]),

    %% Получить мерчанта, связаного с транзакцией
    {ok, [MerchData | _]} = billy_cbserver:get_user(#{user_id => MerchantId, res_type => json}),

    MerchName = proplists:get_value(<<"name">>, MerchData),
    MerchParamsBin = proplists:get_value(<<"params">>, MerchData),
    MerchParams = jiffy:decode(MerchParamsBin, [return_maps]),

    %% io:format("DEBUG>>> billy_ipn_worker(~p):handle_cast TrCost=~p;TrCurrency=~p~n", [self(), TrCost, TrCurrency]),
    %% io:format("DEBUG>>> billy_ipn_worker(~p):handle_cast MerchParams=~p~n", [self(), MerchParams]),

    %% Получить ссылку для оповещения
    ResultUrl = maps:get(<<"result_url">>, MerchParams),

    %% Парсим result_url и определяем метод запроса 
    case http_uri:parse(binary_to_list(ResultUrl)) of
	%% URL валидна, информация получена
	{ok, {ResultUrlProto, _, _ResultUrlHost, _ResultUrlPort, _ResultUrlQuery, _}} ->
	    
	    %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	    %% Сформировать тело IPN
	    
	    %% Получить и декодировать параметры транзакции
	    TrParamsBin = proplists:get_value(<<"params">>, TrData),
	    TrParams = jiffy:decode(TrParamsBin, [return_maps]),

	    %% io:format("DEBUG>>> billy_ipn_worker(~p):handle_cast TrParams=~p~n", [self(), TrParams]),

	    %% Получить id транзакции в системе мерчанта
	    BillId = maps:get(<<"bill_id">>, TrParams), 
	    
	    %% Получить результат обработки транзакции (приняты ли деньги реально или нет)
	    %% Статус самой транзакции говорит о том, обработана они или нет в контексте шлюза и оповещён ли мерчант о результате
	    ProcessResult = maps:get(<<"process_result">>, TrParams, <<"unknown">>),

	    %% Получить сумму и валюту транзакции
	    TrCost = proplists:get_value(<<"cost">>, TrData),
	    FTrCost = list_to_binary(float_to_list(TrCost / 100, [{decimals,2}])),
	    TrCurrency = proplists:get_value(<<"currency_alpha">>, TrData),

	    %% Получить тип транзакции
	    TrType = proplists:get_value(<<"type">>, TrData),
	    BillType = case TrType of
			   1  -> <<"cashin">>; %% Транзакция получения денег
			   40 -> <<"cashout">> %% Транзакция отправки денег
		       end,


	    %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	    %% Подсчитать IPN сигнатуру

	    %% Получить секретный ключ
	    MerchantSecretKey = maps:get(<<"secret_key">>, MerchParams),

	    %% Сформировать список параметров для подсчёта сигнатуры
	    BillyIpnSignParams = [
				  {bill_type, BillType},
				  {bill_id, BillId},
				  {amount, TrCost},
				  {ccy, TrCurrency},
				  {process_result, ProcessResult}
				 ],
	    
	    BillyIpnSign = billy_payment:calc_billyipn_signature(BillyIpnSignParams, MerchantSecretKey),
	    
	    %% io:format("DEBUG>>> billy_ipn_worker(~p):handle_cast BillyIpnSign=~p~n", [self(), BillyIpnSign]),
	    %% io:format("DEBUG>>> billy_ipn_worker(~p):handle_cast ResultUrl=~p~n", [self(), ResultUrl]),
	    
	    Body = io_lib:format("bill_type=~ts&bill_id=~p&amount=~p&ccy=~ts&process_result=~ts",
				 [
				  BillType,
				  BillId,
				  TrCost,
				  TrCurrency,
				  ProcessResult
				 ]),
	    
	    %% io:format("DEBUG>>> billy_ipn_worker(~p):handle_cast Body=~p~n", [self(), Body]),
	    
	    %% Вызвать ссылку для оповещения
	    Headers = [{<<"x-api-signature">>, BillyIpnSign}],
	    Options = [insecure],
	    case hackney:request(post, ResultUrl, Headers, Body, Options) of
		%% Запрос выполнен успешно
		{ok, 200, RespHeaders, ClientRef}=HR ->
		    %% Получить тело ответа и определить что сервер мерчанта закрыл транзакцию
		    {ok, RespBody} = hackney:body(ClientRef),
		    
		    %% io:format("DEBUG>>> billy_ipn_worker(~p):handle_cast HackeyReq:=~p~n", [self(), HR]),
		    %% io:format("DEBUG>>> billy_ipn_worker(~p):handle_cast RespBody:=~p~n", [self(), RespBody]),
		    
		    case RespBody of
			<<"OK">> ->

			    error_logger:info_msg("[200] FINISH IPN >>> merchant: ~ts | +~ts ~ts  (tr_id: ~p)~n", [MerchName, FTrCost, TrCurrency, TrId]),

			    %% Обновить транзакцию в кэше и в базе что она обработана
			    billy_cbserver:update_transaction(#{transaction_id => TrId,
								old_status => TrStatus,
								new_status => 2,
								mode => cache}),
			    
			    billy_cbserver:update_transaction(#{transaction_id => TrId, status => 2});
			RespBody ->

			    error_logger:info_msg("[200] ERROR IPN >>> merchant: ~ts, cost: ~ts ~ts  (tr_id: ~p) RESP: ~ts~n", [MerchName, FTrCost, TrCurrency, TrId, RespBody]),

			    %% Что то там у мерчанта не получается...продолжаем попытки
			    billy_cbserver:update_transaction(#{transaction_id => TrId,
								old_status => TrStatus,
								new_status => 1,
								mode => cache}),
			    
			    billy_cbserver:update_transaction(#{transaction_id => TrId, status => 1})
		    end,
		    ok;
		%% Что-то пошло не так. (404, 500, 301 и.т.д)
		{ok, HttpErrorCode, _RespHeaders, _ClientRef} ->

		    error_logger:info_msg("[~p] ERROR IPN >>> merchant: ~ts, cost: ~ts ~ts  (tr_id: ~p)~n", [HttpErrorCode, MerchName, FTrCost, TrCurrency, TrId]),

		    %% Обновить статус транзакции в кэше и в базе = 6. Холодное обновление
		    %% Транзакции с таким статусом обрабатываются раз в 2 мин.
		    billy_cbserver:update_transaction(#{transaction_id => TrId,
							old_status => TrStatus,
							new_status => 6,
							mode => cache}),
		    
		    billy_cbserver:update_transaction(#{transaction_id => TrId, status => 6}),
		    ok;
		%% Костыль-заглушка для отладки. потом убрать
		Resp ->

		    error_logger:info_msg("DEBUG>>> billy_ipn_worker(~p):handle_cast HTTP ERROR RESPONSE: ~p~n", [self(), Resp]),

		    ok;
		%% Такого домена не существует (received an NXDOMAIN error from a DNS)
		{error, nxdomain} ->
		    %% TODO : Заблокировать пользователя и заморозить все его транзакции 
		    ok;
		%% Ошибка tls=недействительный сертификат. 
		{error, {tls_alert,"handshake failure"}} ->
		    %% TODO : Заблокировать пользователя и заморозить все его транзакции
		    ok
	    end,
	    {noreply, State};
	%% Ошибка парсинга result URL в настройках мерчанта
	ParceResultUrlErr ->
	    %% Обновить статус транзакции как 'ошибочная'
	    %% TODO: запретить мерчанту создавать новые транзакции
	    io:format("DEBUG>>> billy_ipn_worker(~p):handle_cast ParceResultUrlErr=~p~n", [self(), ParceResultUrlErr]),
	    {noreply, State}
    end;


handle_cast(Command, State) ->
    io:format("DEBUG>>> billy_ipn_worker(~p):handle_cast(~p, ~p)~n", [self(), Command, State]),
    {noreply, State}.

handle_info(Info, State) ->
    io:format("DEBUG>>> billy_ipn_worker(~p):handle_info(~p, ~p)~n", [self(), Info, State]),
    {noreply, State}.

terminate(Reason, State) ->
    io:format("DEBUG>>> billy_ipn_worker(~p):terminate(~p, ~p)~n", [self(), Reason, State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================



