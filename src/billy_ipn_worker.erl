-module(billy_ipn_worker).

-behaviour(gen_server).
%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-include("../include/billy_transaction.hrl").


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
    {ok, #{}}.

handle_call(Command, _From, State) ->
    io:format("DEBUG>>> billy_ipn_worker(~p):handle_call(~p, ~p)~n", [self(), Command, State]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast({notify, #{transaction_id := TrId}} = Command, State) ->

    %% Получить транзакцию из базы и проверить статус
    {ok, [Tr | _]} = billy_transaction:get(#{transaction_id => TrId}),

    TrStatus = proplists:get_value(<<"status">>, Tr),
    MerchantId = proplists:get_value(<<"merchant_id">>, Tr),

    {ok, [Merchant | _]} = billy_merchant:get(#{merchant_id => MerchantId}),

    MerchName = proplists:get_value(<<"name">>, Merchant),

    %% Загружаем конфиг мерчанта
    {ok, MerchantConfig} = billy_config:get(#{merchant_id => MerchantId}),
    
    %% Получить ссылку для оповещения
    {ok, [{_, ResultUrl}]} = billy_config:get(#{merchant_config => MerchantConfig, key => "result_url"}),

    %% io:format("DEBUG>>> billy_ipn_worker(~p):handle_cast MerchantConfig=~p~n", [self(), MerchantConfig]),
    %% io:format("DEBUG>>> billy_ipn_worker(~p):handle_cast ResultUrl=~p~n", [self(), ResultUrl]),

    %% Парсим result_url и определяем метод запроса
    case http_uri:parse(binary_to_list(ResultUrl)) of
	%% URL валидна, информация получена
	{ok, {ResultUrlProto, _, _ResultUrlHost, _ResultUrlPort, _ResultUrlQuery, _}} ->
	    
	    %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	    %% Сформировать тело IPN

	    TrData = Tr, %% STUB

	    %% Получить и декодировать параметры транзакции
	    TrParamsBin = proplists:get_value(<<"params">>, TrData),
	    TrParams = jiffy:decode(TrParamsBin, [return_maps]),

	    %% Получить id транзакции в системе мерчанта
	    BillId = maps:get(<<"bill_id">>, TrParams),
	    
	    %% Получить результат обработки транзакции (приняты ли деньги реально или нет)
	    %% Статус самой транзакции говорит о том, обработана они или нет в контексте шлюза и оповещён ли мерчант о результате
	    ProcessResultCode = proplists:get_value(<<"process_result_code">>, TrData, <<"unknown">>),
	    
	    %% Получить сумму и валюту транзакции
	    TrCost = proplists:get_value(<<"amount">>, TrData),
	    FTrCost = list_to_binary(float_to_list(TrCost / 100, [{decimals,2}])),
	    TrCurrency = proplists:get_value(<<"ccy_alpha">>, TrData),

	    %% Получить тип транзакции
	    TrType = proplists:get_value(<<"type">>, TrData),
	    BillType = case TrType of
			   1  -> <<"cashin">>; %% Транзакция получения денег
			   40 -> <<"cashout">> %% Транзакция отправки денег
		       end,

	    %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	    %% Подсчитать IPN сигнатуру

	    %% Получаем секретный ключ мерчанта
	    {ok, [{_, MerchantSecretKey}]} = billy_config:get(#{merchant_config => MerchantConfig, key => "secret_key"}),

	    %% Сформировать список параметров для подсчёта сигнатуры
	    BillyIpnSignParams = [
				  {bill_type, BillType},
				  {bill_id, BillId},
				  {amount, TrCost},
				  {ccy, TrCurrency},
				  {process_result_code, ProcessResultCode}
				 ],
	    
	    BillyIpnSign = billy_payment:calc_billyipn_signature(BillyIpnSignParams, MerchantSecretKey),
	    
	    Body = io_lib:format("bill_type=~ts&bill_id=~p&amount=~p&ccy=~ts&process_result_code=~p",
				 [
				  BillType,
				  BillId,
				  TrCost,
				  TrCurrency,
				  ProcessResultCode
				 ]),

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
		    
		    LocalTime = calendar:local_time(),

		    case RespBody of
			<<"OK">> ->

			    lager:info("(http:200) FINISH IPN >>> merchant: ~ts | +~ts ~ts  (tr_id: ~p)~n", [MerchName, FTrCost, TrCurrency, TrId]),

			    %% Обновить транзакцию в кэше и в базе что она обработана
			    {ok, _} = billy_transaction:update(#{transaction_id => TrId,
								 old_ipn_process_stage => ?IPN_PROCESS_STAGE_INWORK,
								 new_ipn_process_stage => ?IPN_PROCESS_STAGE_COMPLETE,
								 mode => cache}),
			    {ok, 1} = billy_transaction:update(#{transaction_id => TrId, ipn_process_stage => ?IPN_PROCESS_STAGE_COMPLETE, ipn_process_date => LocalTime}),
			    
			    %% Закрыть транзакцию!
			    {ok, 1} = billy_transaction:close(#{transaction_id => TrId});

			RespBody ->

			    lager:info("(http:200) ERROR IPN >>> merchant: ~ts, cost: ~ts ~ts  (tr_id: ~p) RESP: ~ts~n", [MerchName, FTrCost, TrCurrency, TrId, RespBody]),
			    
			    %% Что то там у мерчанта не получается...продолжаем попытки
			    {ok, _} = billy_transaction:update(#{transaction_id => TrId,
								 old_ipn_process_stage => ?IPN_PROCESS_STAGE_INWORK,
								 new_ipn_process_stage => ?IPN_PROCESS_STAGE_COLD,
								 mode => cache}),
			    {ok, 1} = billy_transaction:update(#{transaction_id => TrId, ipn_process_stage => ?IPN_PROCESS_STAGE_COLD, ipn_process_date => LocalTime})
		    end,
		    ok;
		%% Что-то пошло не так. (404, 500, 301 и.т.д)
		{ok, HttpErrorCode, _RespHeaders, _ClientRef} ->

		    LocalTime = calendar:local_time(),

		    lager:info("[http:~p] ERROR IPN >>> merchant: ~ts, cost: ~ts ~ts  (tr_id: ~p)~n", [HttpErrorCode, MerchName, FTrCost, TrCurrency, TrId]),

		    {ok, _} = billy_transaction:update(#{transaction_id => TrId,
							 old_ipn_process_stage => ?IPN_PROCESS_STAGE_INWORK,
							 new_ipn_process_stage => ?IPN_PROCESS_STAGE_COLD,
							 mode => cache}),
		    {ok, 1} = billy_transaction:update(#{transaction_id => TrId, ipn_process_stage => ?IPN_PROCESS_STAGE_COLD, ipn_process_date => LocalTime});

		{error, connect_timeout} ->

		    LocalTime = calendar:local_time(),

		    lager:info("[~p] ERROR IPN >>> merchant: ~ts, cost: ~ts ~ts  (tr_id: ~p)~n", [timeout, MerchName, FTrCost, TrCurrency, TrId]),
		    
		    {ok, _} = billy_transaction:update(#{transaction_id => TrId,
							 old_ipn_process_stage => ?IPN_PROCESS_STAGE_INWORK,
							 new_ipn_process_stage => ?IPN_PROCESS_STAGE_COLD,
							 mode => cache}),
		    {ok, 1} = billy_transaction:update(#{transaction_id => TrId, ipn_process_stage => ?IPN_PROCESS_STAGE_COLD, ipn_process_date => LocalTime});

		%% Костыль-заглушка для отладки. потом убрать
		Resp ->

		    lager:error("ERROR IPN RESPONSE: ~p~n", [Resp]),
		    
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

	    error_logger:info_msg("DEBUG>>> billy_ipn_worker(~p):handle_cast  ParceResultUrlErr=~p~n", [self(), ParceResultUrlErr]),

	    {noreply, State}
    end;


handle_cast(Command, State) ->

    error_logger:info_msg("DEBUG>>> billy_ipn_worker(~p):handle_cast(~p, ~p)~n", [self(),  Command, State]),

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



