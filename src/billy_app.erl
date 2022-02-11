-module(billy_app).
-behaviour(application).

-export([start/2, stop/1]).

-include("../include/billy_transaction.hrl").
-include("../include/rate.hrl").

start(_Type, _Args) ->

    %% Создаём схему БД mnesia
    mnesia_create_schema(),

    %% Запускаем mnesia
    {ok, _} = application:ensure_all_started(mnesia),

    %% Инициализируем mnesia
    mnesia_init_tables(),

    %% Создаём пул процессов для вызова платёжных оповещений (ipn)
    %% wpool:start_sup_pool(billy_ipn_wpool, [{workers, 1}, {worker_type, gen_server}, {worker, {billy_ipn_worker, []}}] ),

    %% %% Создаём пул соединений mysql клиента
    %% emysql:add_pool(main_pool, [
    %% 				{size, 2},
    %% 				{user, billy_config:get(mysql_username)}, {password, billy_config:get(mysql_password)},
    %% 				{host, billy_config:get(mysql_host)}, {port, billy_config:get(mysql_port)}, {database, billy_config:get(mysql_database)}, {encoding,utf8}
    %% 			       ]),

    %% %% Инициализируем prepared statements 
    %% billy_mysql:init_stmt(),
    %% billy_merchant_db:init_stmt(),
    %% billy_transaction_db:init_stmt(),

    %% Загружаем и компилируем таблицу роутинга 
    {ok, Routes} = billy_cow_routes:load_routes(),
    Dispatch = billy_cow_routes:compile_dispatch(Routes),
    Port = billy_config:get(listen_port),
    {ok, _} = cowboy:start_clear(my_http_listener,
				 [{port, Port}],
				 #{env => #{dispatch => Dispatch}}
				),

    %% Запускаем главный супервизор
    billy_sup:start_link().


stop(_State) ->
    ok.


mnesia_create_schema() ->
    MnesiaNodes = [node()],
    case mnesia:create_schema(MnesiaNodes) of
	ok -> 
	    io:format("DEBUG>>> billy:start -> mnesia:create_schema : ok!~n");
	{error, {_,{already_exists, _}}} ->
	    io:format("DEBUG>>> billy:start -> mnesia:create_schema : already_exists!~n")
    end,
    ok.

mnesia_init_tables() ->
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





