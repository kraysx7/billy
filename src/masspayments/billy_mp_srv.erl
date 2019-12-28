-module(billy_mp_srv).
-behaviour(gen_server).


%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(CHECK_MASSPAYMENTS_TIMER, 10000). % start after 10 secs

%%%===================================================================
%%% API
%%%===================================================================

start_link(#{method := Method} = P) ->
    ServerName = list_to_atom(lists:flatten(io_lib:format("~p_~p", [?MODULE, Method]))),
    gen_server:start_link({local, ServerName}, ?MODULE, P, []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(#{method := Method}) ->
    
    erlang:send_after(?CHECK_MASSPAYMENTS_TIMER, self(), {check_mass_payments, Method}),

    {ok, #{}}.


handle_call(Command, _From, State) ->
    io:format("DEBUG>>> billy_mp_srv:undef CALL ~p~n", [Command]),
    Reply = ok,
    {reply, Reply, State}.


handle_cast(Command, State) ->
    io:format("DEBUG>>> billy_mp_srv:undef CAST ~p~n", [Command]),
    {noreply, State}.


handle_info({check_mass_payments, Method}, State) ->
    
    check_mass_payments(#{method => Method}),

    %% io:format("DEBUG>>> billy_mp_srv:info(check_mass_payments ~p) finished!~n", [Method]),

    erlang:send_after(?CHECK_MASSPAYMENTS_TIMER, self(), {check_mass_payments, Method}),

    {noreply, State};


handle_info(Info, State) ->
    case Info of
	Info ->
	    io:format("DEBUG>>> billy_mp_srv:handle_info UNCLAUSE INFO: ~p~n", [Info]),
	    {noreply, State}
    end.

terminate(Reason, State) ->
    io:format("DEBUG>>> billy_mp_srv:terminate Reason=~p~n", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================


check_mass_payments(#{method := Method}) ->
    
    %% Получить список заказов массовых выплат, ожидающих отправки для соотв. системы
    
    MethodBinStr = erlang:atom_to_binary(Method, latin1),
    <<MethodHash:64, _/binary>> = crypto:hash(sha, MethodBinStr),
    
    Params = [0, MethodHash],
    
    case billy_mysql:exec_prepared_stmt(#{stmt => get_masspayment_orders_stmt, params => Params}) of
	{ok, Orders} ->
	    
	    %% Получить модуль-обработчик в зависимости от метода
	    ProcessModule = case Method of
				qiwi -> billy_mp_qiwi_handler;
				steam -> billy_mp_qiwi_handler;
				payeer -> billy_mp_payeer_handler;
				btc -> billy_mp_btc_handler
			    end,
	    
	    %% Пройтись по списку и вызвать соотв. обработчик отправки
	    lists:foreach(fun(Order) -> 
				  MerchantId = proplists:get_value(<<"merchant_id">>, Order),
				  MpOrderId = proplists:get_value(<<"order_id">>, Order),
				  MerchantBillId = proplists:get_value(<<"merchant_bill_id">>, Order),
				  ConfigGroupId = proplists:get_value(<<"method_config_group">>, Order),
				  Amount = proplists:get_value(<<"amount">>, Order),
				  CcyAlpha = proplists:get_value(<<"currency_alpha">>, Order),
				  CcyNumber = proplists:get_value(<<"currency_number">>, Order),
				  Address = proplists:get_value(<<"address">>, Order),
				  
				  ProcessModule:process_payment(#{method => Method, 
								  merchant_id => MerchantId,
								  mp_order_id => MpOrderId, 
								  bill_id => MerchantBillId,
								  address => Address, 
								  config_group_id => ConfigGroupId, 
								  amount => Amount, 
								  ccy_alpha => CcyAlpha, 
								  ccy_number => CcyNumber}),
				  timer:sleep(5000)
			  end, Orders)
    end.
