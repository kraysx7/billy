-module(billy_ipn_srv).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(CHECK_HOT_IPN_TIMER, 10000). % start after 10 secs
-define(CHECK_COLD_IPN_TIMER, 60000). % start after 60 secs

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% add_listen_node(Node) ->
%%     gen_server:call(?SERVER, {add_listen_node, Node}, 5000).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    io:format("DEBUG>>> billy_ipn_srv:init~n"),

    %% Start ipn check timer
    erlang:send_after(?CHECK_HOT_IPN_TIMER, self(), check_hot_ipn),

    erlang:send_after(?CHECK_COLD_IPN_TIMER, self(), check_cold_ipn),

    {ok, #state{}}.


handle_call(Command, _From, State) ->
    io:format("DEBUG>>> billy_ipn_srv:handle_call(~p, ~p)~n", [Command, State]),
    Reply = ok,
    {reply, Reply, State}.


handle_cast(Command, State) ->
    io:format("DEBUG>>> billy_ipn_srv:handle_cast(~p, ~p)~n", [Command, State]),
    {noreply, State}.


handle_info(check_hot_ipn, State) ->
    %% Получить список транзакций требующих горячего оповещения
    Filters = #{status => 1, res_type => json},
    case billy_cbserver:get_transaction(Filters) of
	{ok, Trs} -> 
	    process_ipn(#{transactions => Trs}),
	    ok;
	{error, not_found} -> ok
    end,

    %% io:format("DEBUG>>> billy_ipn_srv:handle_info(check_ipn) finished!~n"),
    erlang:send_after(?CHECK_HOT_IPN_TIMER, self(), check_hot_ipn),
    {noreply, State};

handle_info(check_cold_ipn, State) ->
    %% Получить список транзакций требующих холодного оповещения
    Filters = #{status => 6, res_type => json},
    case billy_cbserver:get_transaction(Filters) of
	{ok, Trs} -> 
	    process_ipn(#{transactions => Trs}),
	    ok;
	{error, not_found} -> ok
    end,

    %% io:format("DEBUG>>> billy_ipn_srv:handle_info(check_ipn) finished!~n"),
    erlang:send_after(?CHECK_COLD_IPN_TIMER, self(), check_cold_ipn),
    {noreply, State};


handle_info(Info, State) ->
    case Info of
	%% {'DOWN', MonitorRef, process, _Pid,  _Reason} ->
	%%     io:format("erlsteam_listener_srv INFO ~p~n", [Info]),
	%%     erlang:demonitor(MonitorRef),
	%%     {ok, Pid} = erlsteam_listener_gen:start(),
	%%     NewMonitorRef = erlang:monitor(process, Pid),
	%%     {noreply, State#state{monitor_ref = NewMonitorRef, erlsteam_gen = Pid}};

	Info ->
	    io:format("DEBUG>>> billy_ipn_srv:handle_info(~p, ~p)~n", [Info, State]),
	    {noreply, State}
    end.

terminate(Reason, State) ->
    io:format("DEBUG>>> billy_ipn_srv:terminate(~p,~p)~n", [Reason, State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================


process_ipn(#{transactions := Trs}) ->
    %% Пройтись по списку
    lists:foreach(fun(Tr) ->
			  TrId = proplists:get_value(<<"transaction_id">>, Tr),
			  TrStatus = proplists:get_value(<<"status">>, Tr),
			  
			  %% io:format("DEBUG>>> billy_ipn_srv:handle_info(check_ipn) PROCESS TrId:TrStatus ~p:~p~n", [TrId,TrStatus]),
			  
			  %% Получить транзакцию из кэша в json формате (mnesia)
			  CacheTrRes = billy_cbserver:get_transaction(#{transaction_id => TrId,
									mode => cache,
									res_type => json}),
			  
			  %% io:format("DEBUG>>> billy_ipn_srv:handle_info(check_ipn) CacheTrRes ~p~n", [CacheTrRes]),
			  
			  %% Проверить статус транзакции по кэшу
			  case CacheTrRes of
				  {ok, CacheTr} ->
				  CacheTrStatus = proplists:get_value(<<"status">>, CacheTr),
				  case CacheTrStatus of
				      %% Транзакция готова к обработке
				       CacheTrStatus when CacheTrStatus==1;CacheTrStatus==6-> 
					  %% Обновить состояние транзакции
					  NewStatus = 5,
					  case billy_cbserver:update_transaction(#{transaction_id => TrId,
										   old_status => CacheTrStatus,
										   new_status => NewStatus,
										   mode => cache}) of
					      {ok, NewStatus} -> 
						  
						  LocalTime = calendar:local_time(),
						  LocalTimeUtc = billy_commons:datetime_to_utc(LocalTime),
						  
						  %% io:format("DEBUG>>> billy_ipn_srv:handle_info(check_ipn) LocalTime: ~p~n", [LocalTime]),
						  %% io:format("DEBUG>>> billy_ipn_srv:handle_info(check_ipn) LocalTimeUtc: ~p~n", [LocalTimeUtc]),
						  
						  %% Помещаем в close_date время начала оповещения
						  ok = billy_cbserver:update_transaction(#{transaction_id => TrId,
											   close_date_utc => LocalTimeUtc,
											   mode => cache}),
						  
						  %% io:format("DEBUG>>> billy_ipn_srv:handle_info(check_ipn) wpool:cast (~p)~n", [TrId]),
						  
						  %% MerchantId = proplists:get_value(<<"user_id">>, CacheTr),
						  NotifyParamsMap = #{transaction_id => TrId},
						  
						  %% Вызвать cast ipn воркера через wpool !
						  wpool:cast(billy_ipn_wpool, {notify, NotifyParamsMap}),
						  
						  ok;
					      %% Ошибка смены статуса..ситуация очень странная до невозможности
					      _ -> ok
					  end;
				      %% Транзакция в процессе обработки
				      5 ->
					  %% Проверить дату начала обработки из параметров
					  %% Дата закрытия в кэше = дата начала обработки
					  TrIpnDate = proplists:get_value(<<"close_date">>, CacheTr),
					  
					  LocalTime = calendar:local_time(),
					  LocalTimeUtc = billy_commons:datetime_to_utc(LocalTime),
					  
					  case LocalTimeUtc-TrIpnDate of
					      %% Прошло больше 30 секунд начала старта обработки, но ничего не произошло...
					      DT when DT > 30 ->
						  %% Меняем статус транзакции в кэше на статус по базе (готова к обработке)
						  {ok, _} = billy_cbserver:update_transaction(#{transaction_id => TrId,
												old_status => 5,
												new_status => TrStatus,
												mode => cache});
					      _ -> ok
					  end;
				      %% По кэшу транзакция уже закрыта, но в базе почему то статус не изменён
				      CacheTrStatus when CacheTrStatus==2;CacheTrStatus==7 ->
					  %% TODO: принудительно обновить статус в базе
					  %% но вообще таких ситуаций возникать не должно, т.к. воркеры обработчики сами
					  %% должны обновлять это состояние..если только не упадут где-то совсем внезапно
					  ok
				  end;
			      {error, not_found} ->
				  %% Нет транзакции..очень странно. это ошибка кэша
				  ok
			  end
		  end, Trs),
    ok.





