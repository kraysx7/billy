-module(billy_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->

    %% Конфигурируем пул соединений mysql клиента
    emysql:add_pool(main_pool, [
				{size, 2},
				{user, billy_config:get(mysql_username)}, {password, billy_config:get(mysql_password)},
				{host, billy_config:get(mysql_host)}, {port, billy_config:get(mysql_port)}, {database, billy_config:get(mysql_database)}, {encoding,utf8}
			       ]),
    billy_mysql:init_stmt(),

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





