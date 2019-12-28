-module(billy_merchant_db).
-author("Ilya Troshkov").

-export([init_stmt/0, get/1, get_config/1, get_merchant_paysystem/1]).

-include("../include/mysql.hrl").


%% @doc Инициализируем хранимые процедуры
init_stmt() ->
    
    %% merchant 

    emysql:prepare(get_merchant_1_stmt, 
		   <<"SELECT * FROM `billy_merchant` WHERE `merchant_id` = ?">>),


    %% merchant config

    emysql:prepare(get_merchant_config_stmt, 
		   <<"SELECT * FROM `billy_merchant_config` WHERE `merchant_id` = ? AND `key` = ?">>),

    emysql:prepare(get_merchant_config_full_stmt, 
		   <<"SELECT * FROM `billy_merchant_config` WHERE `merchant_id` = ?">>),

    %% merchant paysystem

    emysql:prepare(get_paysystem_info_stmt, 
		   <<"SELECT * FROM `billy_paysystem` WHERE `paysystem_id` = ?">>),

    emysql:prepare(get_merchant_paysystem_stmt, 
		   <<"SELECT * FROM `billy_merchant_paysystem` WHERE `merchant_id` = ? AND `paysystem_id` = ?">>),
    ok.


%%%================= Функции для работы с мерчантами и конфигурациями =================

%% @doc Получает описание мерчанта по merchant_id
get(#{merchant_id := MerchantId}) ->
    GetParams = [MerchantId],
    billy_mysql:exec_prepared_stmt(#{stmt => get_merchant_1_stmt, params => GetParams}).


%% @doc Получает конфиг мерчанта по заданному ключу
get_config(#{merchant_id := MerchantId, key := Key}) ->
    GetParams = [MerchantId, Key],
    billy_mysql:exec_prepared_stmt(#{stmt => get_merchant_config_stmt, params => GetParams});

%% @doc Получает весь конфиг мерчанта
get_config(#{merchant_id := MerchantId}) ->
    GetParams = [MerchantId],
    billy_mysql:exec_prepared_stmt(#{stmt => get_merchant_config_full_stmt, params => GetParams}).


%%%================= Функции для работы с платёжными системами ========================

get_merchant_paysystem(#{merchant_id := MerchantId, paysystem_id := PaySystemId}) ->
    GetParams = [MerchantId, PaySystemId],
    billy_mysql:exec_prepared_stmt(#{stmt => get_merchant_paysystem_stmt, params => GetParams}). 
