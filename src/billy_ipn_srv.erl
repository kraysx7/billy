-module(billy_ipn_srv).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("../include/billy_transaction.hrl").

-define(SERVER, ?MODULE).

-define(CHECK_HOT_IPN_TIMER, 30000). % start after 10 secs
-define(CHECK_COLD_IPN_TIMER, 30000). % start after 60 secs

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->

    erlang:send_after(?CHECK_HOT_IPN_TIMER, self(), check_hot_ipn),

    erlang:send_after(?CHECK_COLD_IPN_TIMER, self(), check_cold_ipn),

    {ok, #state{}}.


handle_call(Command, _From, State) ->
    {reply, ok, State}.

handle_cast(Command, State) ->
    {noreply, State}.


handle_info(check_hot_ipn, State) ->
    %% Получить список транзакций требующих горячего оповещения (tr.ipn_process_stage=1)
    case billy_transaction:get(#{ipn_process_stage => ?IPN_PROCESS_STAGE_HOT, status => 1}) of
	{ok, Trs} -> process_ipn(#{transactions => Trs});
	{error, not_found} -> ok
    end,

    erlang:send_after(?CHECK_HOT_IPN_TIMER, self(), check_hot_ipn),
    {noreply, State};

handle_info(check_cold_ipn, State) ->
    %% Получить список транзакций требующих холодного оповещения (tr.ipn_process_stage=2)
    case billy_transaction:get(#{ipn_process_stage => ?IPN_PROCESS_STAGE_COLD, status => 1}) of
	{ok, Trs} -> process_ipn(#{transactions => Trs});
	{error, not_found} -> ok
    end,

    erlang:send_after(?CHECK_COLD_IPN_TIMER, self(), check_cold_ipn),
    {noreply, State};


handle_info(Info, State) ->
    io:format("DEBUG>>> billy_ipn_srv:handle_info(~p, ~p)~n", [Info, State]),
    {noreply, State}.


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
    lists:foreach(fun(DbTr) ->
			  %% Получить транзакцию из кэша
 			  TrId = proplists:get_value(<<"transaction_id">>, DbTr),
			  {ok, CacheTr} = billy_transaction:get(#{transaction_id => TrId, mode => cache}),
			  			  
			  %% Проверить текущий этап ipn
			  CurIpnStage = proplists:get_value(<<"ipn_process_stage">>, CacheTr),
			  case CurIpnStage of
			      
			      %% Транзакция готова к ipn
			      CurIpnStage when CurIpnStage==?IPN_PROCESS_STAGE_HOT;
					       CurIpnStage==?IPN_PROCESS_STAGE_COLD ->
				  
				  %% Обновить текущий этап ipn в кэше
				  NewIpnStage = ?IPN_PROCESS_STAGE_INWORK,
				  case billy_transaction:update(#{transaction_id => TrId,
								  old_ipn_process_stage => CurIpnStage,
								  new_ipn_process_stage => NewIpnStage,
								  mode => cache}) of
				      {ok, NewIpnStage} -> 
					  
					  LocalTime = calendar:local_time(),
					  
					  %% Обновить дату начала ipn в кэше
					  ok = billy_transaction:update(#{transaction_id => TrId, ipn_process_date => LocalTime, mode => cache}),
					  
					  %% Обновить текущий этап ipn и дату начала в бд
					  {ok, 1} = billy_transaction:update(#{transaction_id => TrId, ipn_process_stage => NewIpnStage, ipn_process_date => LocalTime}),
					  
					  %% Вызвать cast ipn воркера через wpool
					  NotifyParamsMap = #{transaction_id => TrId},
					  wpool:cast(billy_ipn_wpool, {notify, NotifyParamsMap})

				  end;
			      
			      %% Транзакция в процессе ipn оповещения (по кэшу)
			      ?IPN_PROCESS_STAGE_INWORK ->
				  
				  DbTrStatus = proplists:get_value(<<"transaction_id">>, DbTr),
				  
				  %% Проверить дату начала обработки
				  IpnProcessDate = proplists:get_value(<<"ipn_process_date">>, CacheTr),
				  LocalTime = calendar:local_time(),
				  LocalTimeUtc = billy_commons:datetime_to_utc(LocalTime),
				  IpnProcessDateUtc = billy_commons:datetime_to_utc(IpnProcessDate),
				  
				  case LocalTimeUtc-IpnProcessDateUtc of
				      
				      %% Прошло больше 30 секунд начала старта обработки, но ничего не произошло...
				      DT when DT > 30 ->
					  
					  %% Меняем статус транзакции в кэше на статус по базе (готова к обработке)
					  {ok, _} = billy_transaction:update(#{transaction_id => TrId,
									       old_ipn_process_stage => ?IPN_PROCESS_STAGE_INWORK,
									       new_ipn_process_stage => DbTrStatus,
									       mode => cache});
				      _ -> ok
				  end
			      %% По кэшу транзакция уже закрыта, но в базе почему то статус не изменён
			      %% CacheTrStatus when CacheTrStatus==2;CacheTrStatus==7 ->
			      %% 	  %% TODO: принудительно обновить статус в базе
			      %% 	  %% но вообще таких ситуаций возникать не должно, т.к. воркеры обработчики сами
			      %% 	  %% должны обновлять это состояние..если только не упадут где-то совсем внезапно
			      %% 	  ok
			  end
		  end, Trs),
    ok.





