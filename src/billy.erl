-module(billy).

-export([start/0, stop/0]).

-include("../include/billy_transaction.hrl").
-include("../include/rate.hrl").

start() ->
    {ok, _} = application:ensure_all_started(sasl),
    {ok, _} = application:ensure_all_started(crypto),

    {ok, _} = application:ensure_all_started(iconv),

    %% Запускаем приложение для входящих Web запросов
    {ok, _} = application:ensure_all_started(cowboy),

    %% Запускаем HTTP(S) клиент hackney для исходящих
    {ok, _} = application:ensure_all_started(public_key),
    {ok, _} = application:ensure_all_started(ssl),
    {ok, _} = application:ensure_all_started(hackney),

    %% Создаём схему БД mnesia
    MnesiaNodes = [node()],
    case mnesia:create_schema(MnesiaNodes) of
	ok -> 
	    io:format("DEBUG>>> billy:start -> mnesia:create_schema : ok!~n");
	{error, {_,{already_exists, _}}} ->
	    io:format("DEBUG>>> billy:start -> mnesia:create_schema : already_exists!~n")
    end,

    %% Запускаем mnesia
    {ok, _} = application:ensure_all_started(mnesia),

    %% Инициализируем mnesia
    init_mnesia(),

    %% Запускаем mysql клиент
    {ok, _} = application:ensure_all_started(emysql),

    %% %% Запускаем worker_pool (inaka)
    %% wpool:start(),

    %% %% Создаём пул процессов для вызова платёжных оповещений мерчантов
    %% wpool:start_sup_pool(billy_ipn_wpool, [{workers, 1}, {worker_type, gen_server}, {worker, {billy_ipn_worker, []}}] ),

    %% Запускаем приложение интерпретатор для php
    ephp:start(),

    %% Запускаем основное приложение
    {ok, _} = application:ensure_all_started(billy),

    ok.


stop() ->
    %% Останавливаем главный каскад в обратном порядке запуска
    ok. %% = stop_apps(lists:reverse(?WEB_APPS)).


init_mnesia() ->
    %% Создаём схему кэш таблички transaction
    mnesia:create_table(transaction_cache,
     			[{attributes, record_info(fields, billy_transaction)},
     			 {record_name, billy_transaction},
     			 {type, set}]),
    
    %% Создаём схему кэш таблички cbrates (текущие(daily) котировки центрального банка)
    mnesia:create_table(cbrates,
     			[{attributes, record_info(fields, rate)},
     			 {record_name, rate},
     			 {type, set}]),
    
    mnesia:wait_for_tables([cbrates], 5000),
    
    ok.


%% init_mnesia() ->
%%     %% Создаём схему кэш таблички user_service
%%     mnesia:create_table(user_service_cache,
%%      			[{attributes, record_info(fields, user_service)},
%%      			 {record_name, user_service},
%%      			 {type, set}]),
    
%%     mnesia:create_table(user_service_params_cache,
%%      			[{attributes, record_info(fields, user_service_param)},
%%      			 {record_name, user_service_param},
%%      			 {type, bag}]),
    
%%     mnesia:wait_for_tables([user_service_cache,
%% 			    user_service_params_cache], 5000),
    
%%     %% Создаём схему кэш таблички transaction
%%     mnesia:create_table(transaction_cache,
%%      			[{attributes, record_info(fields, transaction)},
%%      			 {record_name, transaction},
%%      			 {type, set}]),    

%%     %% Создаём схему кэш таблички cbrates (текущие(daily) котировки центрального банка)
%%     mnesia:create_table(cbrates,
%%      			[{attributes, record_info(fields, rate)},
%%      			 {record_name, rate},
%%      			 {type, set}]),
    
%%     mnesia:wait_for_tables([cbrates], 5000),

%%     %% Создаём схему кэш таблички user_promocode
%%     mnesia:create_table(user_promocode_cache,
%%      			[{attributes, record_info(fields, user_promocode)},
%%      			 {record_name, user_promocode},
%%      			 {type, set}]),
    
%%     mnesia:wait_for_tables([user_promocode_cache], 5000),

%%     %% CreateDate1 = calendar:datetime_to_gregorian_seconds({{2015, 1, 1}, {0, 0, 0}}),
%%     %% CreateDate2 = calendar:datetime_to_gregorian_seconds({{2016, 5, 1}, {0, 0, 0}}),
%%     %% CreateDate3 = calendar:datetime_to_gregorian_seconds({{2016, 2, 1}, {0, 0, 0}}),
%%     %% CreateDate4 = calendar:datetime_to_gregorian_seconds({{2016, 1, 1}, {0, 0, 0}}),

%%     %% user_service:insert_cache(#user_service{user_service_id=45, type=3001, create_date=CreateDate1, status=1}),
%%     %% user_service:insert_cache(#user_service{user_service_id=46, type=3002, create_date=CreateDate1, status=6}),
%%     %% user_service:insert_cache(#user_service{user_service_id=47, type=3001, create_date=CreateDate3, status=1}),
%%     %% user_service:insert_cache(#user_service{user_service_id=48, type=3002, create_date=CreateDate2, status=6}),
%%     %% user_service:insert_cache(#user_service{user_service_id=49, type=3003, create_date=CreateDate4, status=1}),

%%     io:format("DEBUG>>> main_app:init_mnesia create tables complite!~n"),
%%     ok.



%% ===================================================================
%% Internal functions
%% ===================================================================

%% start_apps([]) -> ok;
%% start_apps([App | Apps]) ->

%%     io:format("DEBUG>>> billy:start_app ~p~n", [App]),
    
%%     case application:start(App) of
%% 	ok -> start_apps(Apps);
%% 	{error, {already_started, App}} -> start_apps(Apps)
%%     end.

%% stop_apps([]) -> ok;
%% stop_apps([App | Apps]) ->
%%     application:stop(App),
%%     stop_apps(Apps).
