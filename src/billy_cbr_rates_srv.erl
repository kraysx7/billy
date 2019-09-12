-module(billy_cbr_rates_srv).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("../include/rate.hrl").

-include_lib("xmerl/include/xmerl.hrl"). 

-define(SERVER, ?MODULE).

-define(SYNC_CENTRALBANK_TIMER, 30000). % sync every 30 seconds

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
    io:format("rates_srv:init CALL~n"),

    %% Сформировать рекорд базовой валюты
    Rate = #rate{
	      currency_number=643,
	      currency_alpha=unicode:characters_to_binary("RUB"),
	      nominal=1,
	      name=unicode:characters_to_binary("Russian Ruble"),
	      value=1
	     },
    
    io:format("DEBUG>>> billy_cbr_rates_srv:init ~p ~ts=~p RUB ____ (~ts)~n", [Rate#rate.nominal, Rate#rate.currency_alpha, Rate#rate.value, Rate#rate.name]),
    
    %% Записать в mnesia
    write_rate(Rate),

    %% Start sync_centralbank timer
    erlang:send_after(15000, self(), sync_centralbank),
    {ok, #state{}}.


handle_call(Command, _From, State) ->
    io:format("rates_srv:undef CALL ~p~n", [Command]),
    Reply = ok,
    {reply, Reply, State}.


handle_cast(Command, State) ->
    io:format("rates_srv:undef CAST ~p~n", [Command]),
    {noreply, State}.


handle_info(sync_centralbank, State) ->
    %% Событие синхронизации текущих котировок ЦБ
    
    {{Y, M, D}, {_, _, _}} = calendar:local_time(),
    Query = io_lib:format("http://www.cbr.ru/scripts/XML_daily.asp?date_req=~2..0B/~2..0B/~p", [D, M, Y]),

    io:format("DEBUG>>> billy_cbr_rates_srv:handle_info {sync_centralbank} Query: ~ts~n", [Query]),
    
    Headers = [],
    Body = <<>>,
    Options = [],
    
    case hackney:request(get, Query, Headers, Body, Options) of
	%% Запрос выполнен успешно
	{ok, 200, RespHeaders, ClientRef}=HR ->

	    %% Получить тело ответа и определить что сервер мерчанта закрыл транзакцию
	    {ok, RespBody} = hackney:body(ClientRef),

	    FinBodyUtf8 = case binary:match(RespBody, <<"<?xml version=\"1.0\" encoding=\"windows-1251\"?">>) of
			      nomatch -> 
				  case binary:match(RespBody, <<"<?xml version=\"1.0\" encoding=\"utf-8\"?">>) of
				      {_, _} -> binary_to_list(RespBody)
				  end;
			      {_, _} -> 
				  RespBodyUtf8 = binary_to_list(iconv:convert("cp1251", "utf-8", RespBody)),
				  re:replace(RespBodyUtf8, "encoding=\"windows-1251\"", "encoding=\"utf-8\"", [{return,list}])
			  end,
	    
	    {Xml, _} = xmerl_scan:string(FinBodyUtf8),
	    
	    XmlRates = xmerl_xpath:string("/ValCurs/Valute", Xml),
	    
	    io:format("DEBUG>>> billy_cbr_rates_srv:sync_centralbank XmlRates len: ~p~n", [length(XmlRates)]),
	    
	    lists:foreach(fun(E) ->
				  NumCode = xml_val(xmerl_xpath:string("NumCode", E)),
				  CharCode = xml_val(xmerl_xpath:string("CharCode", E)),
				  Nominal = xml_val(xmerl_xpath:string("Nominal", E)),
				  Name = xml_val(xmerl_xpath:string("Name", E)),
				  Value0 = xml_val(xmerl_xpath:string("Value", E)),
				  Value = re:replace(Value0, ",", ".", [global, {return, list}]),
				  
				  %% io:format("DEBUG>>> rates_srv:sync_centralbank Value: ~ts~n", [Value]),
				  
				  %% Сформировать рекорд
				  Rate = #rate{
					    currency_number=billy_commons:list_to_integer(NumCode),
					    currency_alpha=unicode:characters_to_binary(CharCode),
					    nominal=billy_commons:list_to_integer(Nominal),
					    name=unicode:characters_to_binary(Name),
					    value=billy_commons:list_to_float(Value)
					   },
				  
				  io:format("DEBUG>>> billy_cbr_rates_srv:sync_centralbank ~p ~ts=~p RUB ____ (~ts)~n", [Rate#rate.nominal, Rate#rate.currency_alpha, Rate#rate.value, Rate#rate.name]),
				  
				  %% Записать в mnesia
				  write_rate(Rate)
			  end, XmlRates),
	    ok;
	
	%% Любой другой ответ
	Resp ->
	    
	    error_logger:info_msg("DEBUG>>> billy_cbr_rates_srv:sync_centralbank HTTP ERROR RESPONSE: ~p~n", [self(), Resp]),
	    
	    ok
    end,
    
    io:format("DEBUG>>> billy_cbr_rates_srv:sync_centralbank finished! get_rate: ~p~n", [billy_cbr_rate:get_rate(#{currency_alpha => <<"EUR">>})]),
    erlang:send_after(?SYNC_CENTRALBANK_TIMER, self(), sync_centralbank),
    {noreply, State};


handle_info(Info, State) ->
    case Info of
	%% {'DOWN', MonitorRef, process, _Pid,  _Reason} ->
	%%     io:format("erlsteam_listener_srv INFO ~p~n", [Info]),
	%%     erlang:demonitor(MonitorRef),
	%%     {ok, Pid} = erlsteam_listener_gen:start(),
	%%     NewMonitorRef = erlang:monitor(process, Pid),
	%%     {noreply, State#state{monitor_ref = NewMonitorRef, erlsteam_gen = Pid}};

	%% Получить сообщение от удалённого узла о событии
	%%

	Info ->
	    io:format("rates_srv:undef INFO~n"),
	    {noreply, State}
    end.

terminate(_Reason, State) ->
    io:format("rates_srv TERMINATE ~p~n", [State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================


xml_val(X) ->
    [#xmlElement{name = _N, content = [#xmlText{value = V}|_]}] = X,
    V.



write_rate(Rate) when is_record(Rate, rate) ->
    F = fun() -> mnesia:write(cbrates, Rate, write) end,
    mnesia:activity(transaction, F).
