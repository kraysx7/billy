-module(billy_transaction_db).

-export([init_stmt/0, create/1, get/1, update/1, close/1]).

-include("../include/mysql.hrl").

-include("../include/billy_transaction.hrl").

%% @doc Инициализируем хранимые процедуры
init_stmt() ->
    
    emysql:prepare(create_transaction_stmt, <<"CALL `create_transaction`(?, ?, ?, ?, ?, ?);">>),

    emysql:prepare(get_transaction_1_stmt, <<"SELECT * FROM `billy_transaction` WHERE `ipn_process_stage` = ? AND `status` = ?">>),

    emysql:prepare(get_transaction_2_stmt, <<"SELECT * FROM `billy_transaction` WHERE `transaction_id` = ?">>),

    emysql:prepare(update_transaction_1_stmt, <<"UPDATE `billy_transaction` SET `ipn_process_stage` = ?, `ipn_process_date` = ? WHERE `transaction_id` = ?">>),

    emysql:prepare(update_transaction_2_stmt, <<"UPDATE `billy_transaction` SET `process_result_code` = ?, `status` = ? WHERE `transaction_id` = ?">>),

    emysql:prepare(update_transaction_3_stmt, <<"UPDATE `billy_transaction` SET `params` = ? WHERE `transaction_id` = ?">>),

    emysql:prepare(close_transaction_stmt, <<"UPDATE `billy_transaction` SET `close_date` = ?, `status` = ? WHERE `transaction_id` = ?">>),

    ok.


%%%================= Функции для работы с транзакциями в БД =================


%% @doc Создаёт транзакцию в бд
create(#{merchant_id := MerchantId, type := Type, amount := Amount, ccy_alpha := CcyAlpha, ccy_number := CcyNumber, params := Params}) ->
    CreateTrParams = [MerchantId, Type, Amount, CcyAlpha, CcyNumber, Params],
    billy_mysql:exec_prepared_stmt(#{stmt => create_transaction_stmt, params => CreateTrParams}).



%% @doc Получает список транзакций по состоянию ipn обработки и статусу
get(#{ipn_process_stage := IpnProcessStage, status := Status}) ->
    GetParams = [IpnProcessStage, Status],
    billy_mysql:exec_prepared_stmt(#{stmt => get_transaction_1_stmt, params => GetParams});

%% @doc Получает список транзакций по id
get(#{transaction_id := TrId}) ->
    GetParams = [TrId],
    billy_mysql:exec_prepared_stmt(#{stmt => get_transaction_2_stmt, params => GetParams}).



%% @doc Обновляет текущий шаг и дату обработки ipn
update(#{transaction_id := TrId, ipn_process_stage := IpnProcessStage, ipn_process_date := IpnProcessDate}) ->
    UpdateParams = [IpnProcessStage, IpnProcessDate, TrId],
    billy_mysql:exec_prepared_stmt(#{stmt => update_transaction_1_stmt, params => UpdateParams});

update(#{transaction_id := TrId, process_result_code := ProcessResultCode, status := Status}) ->
    UpdateParams = [ProcessResultCode, Status, TrId],
    billy_mysql:exec_prepared_stmt(#{stmt => update_transaction_2_stmt, params => UpdateParams});

update(#{transaction_id := TrId, params := Params}) ->
    UpdateParams = [Params, TrId],
    billy_mysql:exec_prepared_stmt(#{stmt => update_transaction_3_stmt, params => UpdateParams}).


%% @doc Закрывает транзакцию в бд
close(#{transaction_id := TrId}) ->
    CloseTrParams = [calendar:local_time(), ?TR_STATUS_CLOSED, TrId],
    billy_mysql:exec_prepared_stmt(#{stmt => close_transaction_stmt, params => CloseTrParams}).


%% -----------------------------------------------
%% NOT REORGANIZED

%% get(#{user_id := UserId, type := Type, currency_number := CurrencyNumber, status := Status, close_date_from := CloseDateFrom}) ->
%%     emysql:prepare(get_trs_full_1_stmt, <<"SELECT * FROM `transaction` WHERE `user_id` = ? AND `type` = ? AND `currency_number` = ? AND `status` = ? AND `close_date` > ?">>),
%%     Result = emysql:execute(main_pool, get_trs_full_1_stmt, [UserId, Type, CurrencyNumber, Status, CloseDateFrom]),

%%     RecResult = emysql:as_record(Result, transaction, record_info(fields, transaction)), 
%%     case RecResult of
%% 	[Tr | _] when is_record(Tr, transaction) ->
%% 	    {ok, RecResult};
%% 	[] -> {error, not_found};
%% 	_  -> {error, undefined}
%%     end;


%% get(#{user_id := UserId, type := Type, currency_number := CurrencyNumber, status := Status, res_type := ResType}) ->
%%     emysql:prepare(get_trs_stmt, <<"SELECT * FROM `transaction` WHERE `user_id` = ? AND `type` = ? AND `currency_number` = ? AND `status` = ?">>),
%%     Result = emysql:execute(main_pool, get_trs_stmt, [UserId, Type, CurrencyNumber, Status]),
%%     build_query_result(#{result => Result, res_type => ResType});


%% get(#{user_id := UserId, type := [S1,S2,S3]}) ->
%%     emysql:prepare(get_trs_by_user_and_type_stmt, <<"SELECT * FROM `transaction` WHERE `user_id` = ? AND (type = ? OR type = ? OR type = ?)">>),
%%     Result = emysql:execute(main_pool, get_trs_by_user_and_type_stmt, [UserId, S1, S2, S3]),

%%     RecResult = emysql:as_record(Result, transaction, record_info(fields, transaction)), 
%%     case RecResult of
%% 	[Tr | _] when is_record(Tr, transaction) ->
%% 	    {ok, RecResult};
%% 	[] -> {error, not_found};
%% 	_  -> {error, undefined}
%%     end;

%% get(#{type := Type, close_date := CloseDate}) ->
%%     emysql:prepare(get_trs_by_close_date_stmt, <<"SELECT * FROM `transaction` WHERE `type` = ? AND `close_date` = ?">>),
%%     Result = emysql:execute(main_pool, get_trs_by_close_date_stmt, [Type, CloseDate]),

%%     RecResult = emysql:as_record(Result, transaction, record_info(fields, transaction)), 
%%     case RecResult of
%% 	[Tr | _] when is_record(Tr, transaction) ->
%% 	    {ok, RecResult};
%% 	[] -> {error, not_found};
%% 	_  -> {error, undefined}
%%     end;


%% get(#{status := Status, res_type := json}) ->
%%     emysql:prepare(get_trs_by_status_jsonstmt, <<"SELECT * FROM `transaction` WHERE `status` = ?">>),
%%     Result = emysql:execute(main_pool, get_trs_by_status_jsonstmt, [Status]),
%%     case Result of
%% 	Result when is_record(Result, result_packet) ->
%% 	    ResultJSON = emysql:as_json(Result),
%% 	    {ok, ResultJSON}
%%     end;

%% get(#{transaction_id := TrId}) ->
%%     emysql:prepare(get_trs_by_tr_id_stmt, <<"SELECT * FROM `transaction` WHERE `transaction_id` = ?">>),
%%     Result = emysql:execute(main_pool, get_trs_by_tr_id_stmt, [TrId]),

%%     RecResult = emysql:as_record(Result, transaction, record_info(fields, transaction)), 
%%     case RecResult of
%% 	[Tr | _] when is_record(Tr, transaction) -> 
%% 	    {ok, Tr};
%% 	[] -> {error, not_found};
%% 	_  -> {error, undefined}
%%     end;


%% get(#{user_id := UserId}) ->
%%     emysql:prepare(get_trs_by_user_id_stmt, <<"SELECT * FROM `transaction` WHERE `user_id` = ?">>),
%%     Result = emysql:execute(main_pool, get_trs_by_user_id_stmt, [UserId]),

%%     RecResult = emysql:as_record(Result, transaction, record_info(fields, transaction)), 
%%     case RecResult of
%% 	[Tr | _] when is_record(Tr, transaction) ->
%% 	    {ok, RecResult};
%% 	[] -> {error, not_found};
%% 	_  -> {error, undefined}
%%     end.



%% get_cost_summ(#{user_id := UserId, type := Type, status := Status}) ->
%%     emysql:prepare(get_trs_cost_summ_stmt, <<"SELECT SUM(cost) AS `cost_summ` FROM `transaction` WHERE `user_id` = ? AND `type` = ? AND `status` = ? AND `currency_number`!=20000">>),
%%     Result = emysql:execute(main_pool, get_trs_cost_summ_stmt, [UserId, Type, Status]),
%%     case Result of
%% 	Result when is_record(Result, result_packet) -> emysql:as_json(Result)
%%     end.


%% update(#{transaction_id := TrId, params := Params}) ->
%%     io:format("DEBUG>>> transaction_db:update params val: ~ts~n", [Params]),
%%     emysql:prepare(update_tr_params_stmt, <<"UPDATE `transaction` SET `params` = ? WHERE `transaction_id` = ?">>),
%%     Result = emysql:execute(main_pool, update_tr_params_stmt, [Params, TrId]),
%%     io:format("DEBUG>>> transaction_db:update params res: ~p~n", [Result]),
%%     case Result of
%% 	OkPacket when is_record(OkPacket, ok_packet) -> ok;
%% 	_  -> {error, db_error}
%%     end;

%% update(#{transaction_id := TrId, cost := Cost}) ->
%%     emysql:prepare(update_tr_cost_stmt, <<"UPDATE `transaction` SET `cost` = ? WHERE `transaction_id` = ?">>),
%%     Result = emysql:execute(main_pool, update_tr_cost_stmt, [Cost, TrId]),
%%     case Result of
%% 	OkPacket when is_record(OkPacket, ok_packet) -> ok;
%% 	_  -> {error, db_error}
%%     end;

%% update(#{transaction_id := TrId, status := Status}) ->
%%     emysql:prepare(update_tr_status_stmt, <<"UPDATE `transaction` SET `status` = ? WHERE `transaction_id` = ?">>),
%%     Result = emysql:execute(main_pool, update_tr_status_stmt, [Status, TrId]),
%%     case Result of
%% 	OkPacket when is_record(OkPacket, ok_packet) -> ok;
%% 	_  -> {error, db_error}
%%     end.



%% get_first_payments() ->
%%     emysql:prepare(get_first_payments_stmt, <<"SELECT u.name,tr.cost,tr.close_date FROM `transaction` tr INNER JOIN `user` u ON u.user_id=tr.user_id WHERE tr.`type`=10 AND `close_date` IS NOT NULL GROUP BY tr.`user_id` ORDER BY `close_date` DESC">>),
%%     Result = emysql:execute(main_pool, get_first_payments_stmt, []),

%%     case Result of
%% 	Result when is_record(Result, result_packet) -> 
%% 	    FinRows = lists:foldl(fun(Row, Rows) -> 
%% 					  [Name, Cost, Date] = Row,
%% 					  A = [{name, Name}, {cost, Cost}, {date, Date}],
%% 					  lists:append([A], Rows)
%% 				  end, [], Result#result_packet.rows),
%% 	    {ok, FinRows};
%% 	_  -> {error, db_error}
%%     end.


%% set_status(TrId, Status) ->
%%     emysql:prepare(set_tr_status_stmt, <<"UPDATE `transaction` SET `status` = ? WHERE `transaction_id` = ?">>),
%%     Result = emysql:execute(main_pool, set_tr_status_stmt, [Status, TrId]),
%%     case Result of
%% 	OkPacket when is_record(OkPacket, ok_packet) -> ok;
%% 	_  -> {error, db_error}
%%     end.

%% %%%===================================================================
%% %%% Internal functions
%% %%%===================================================================


%% build_query_result(#{result := Result, res_type := record}) ->
%%     RecResult = emysql:as_record(Result, transaction, record_info(fields, transaction)),
%%     case RecResult of
%% 	[Tr | _] when is_record(Tr, transaction) ->
%% 	    {ok, RecResult};
%% 	[] -> {error, not_found};
%% 	_  -> {error, undefined}
%%     end;
%% build_query_result(#{result := Result, res_type := json}) ->
%%     case Result of
%% 	Result when is_record(Result, result_packet) ->
%% 	    JSONResult = emysql:as_json(Result),	    
%% 	    {ok, JSONResult}
%%     end.



%% decode_params(Records) when is_list(Records) ->
%%     lists:foldl(
%%       fun(Record, R) ->
%% 	      UpdatedRecord = decode_params(Record),
%% 	      lists:append(R, [UpdatedRecord])
%%       end, [], Records);

%% decode_params(Record) when is_record(Record, transaction) ->
%%     TermParams = commons:decode_service_params(Record#transaction.params),
%%     Record#transaction{params=TermParams}.

