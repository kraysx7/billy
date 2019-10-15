-module(billy_mysql).

-export([init_stmt/0, exec_query/1, exec_prepared_stmt/1]).

-include("../include/mysql.hrl").


init_stmt() ->

    %% masspayments

    emysql:prepare(create_masspayment_order_stmt, 
		   <<"CALL `create_masspayment_order`(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);">>),
    
    emysql:prepare(get_masspayment_orders_stmt, 
		   <<"SELECT * FROM `billy_masspayment_order` WHERE `status` = ? AND `method_hash` = ?">>),

    emysql:prepare(close_masspayment_order_stmt, 
		   <<"UPDATE `billy_masspayment_order` SET `close_date` = ?, `status` = ? WHERE `order_id` = ?">>),


    ok.



exec_query(#{query := Query}) ->
    Result = emysql:execute(main_pool, Query, 60000),
    case Result of
	Result when is_record(Result, result_packet) -> 
	    ResultJSON = emysql:as_json(Result),
	    {ok, ResultJSON};
	Result when is_record(Result, ok_packet) -> 
	    {ok, Result#ok_packet.affected_rows};
	Result  -> Result
    end.


exec_prepared_stmt(#{stmt := Stmt, params := Params}) ->
    Result = emysql:execute(main_pool, Stmt, Params),
    case Result of
	Result when is_record(Result, result_packet) -> 
	    ResultJSON = emysql:as_json(Result),
	    {ok, ResultJSON};

	Result when is_record(Result, ok_packet) -> 
	    {ok, Result#ok_packet.affected_rows};
	
	[ResultHead, _] when is_record(ResultHead, result_packet) ->
	    if
		length(ResultHead#result_packet.rows) > 0 ->
		    [[Id | _] | _] = ResultHead#result_packet.rows,
		    {ok, Id};
		true -> {error, db_error}
	    end;
	
	Result -> Result
    end.

