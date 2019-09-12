-module(billy_php_handler).

-export([init/2]).

-include("../include/ephp.hrl").

init(Req, OptsMap) ->

    io:format("DEBUG>>> billy_php_handler:init Req = ~p~n", [Req]),

    #{php_file := PhpFile} = OptsMap,
    Filename = io_lib:format("../priv/ephp/~ts", [PhpFile]),
    
    %% Читаем и выполняем php файл
    Body = case file:read_file(Filename) of
	       {ok, Content} ->
		   %% AbsFilename = list_to_binary(filename:absname(Filename)),
		   {ok, Ctx} = ephp:context_new(),
		   ephp:register_module(Ctx, billy_ephp_lib),

		   {ok, Output} = ephp_output:start_link(Ctx, false),
		   ephp_context:set_output_handler(Ctx, Output),

		   {ok, ReqParamsKV, _} = cowboy_req:read_urlencoded_body(Req),
		   PhpPostVar = ephp_array:from_list(ReqParamsKV),
		   ephp_context:set(Ctx, #variable{name = <<"_POST">>}, PhpPostVar),

		   io:format("DEBUG>>> billy_php_handler:init PhpPostVar = ~p~n", [PhpPostVar]),

		   QsParamsKV = cowboy_req:parse_qs(Req),
		   PhpGetVar = ephp_array:from_list(QsParamsKV),
		   ephp_context:set(Ctx, #variable{name = <<"_GET">>}, PhpGetVar),

		   try ephp:eval(Ctx, Content) of
		       {ok, Return} ->
			   Result0 = ephp_context:get_output(Ctx),
			   Result = io_lib:format("~ts", [Result0]), 
			   unicode:characters_to_binary(Result, utf8);
		       {error, EvalErrReason, Index, File, Level, Data} ->
			   list_to_binary(io_lib:format("Process php error: ~p : ~ts : ~p ; Level=~p ; Data=~p", [EvalErrReason, PhpFile, Index, Level, Data]))
		   catch
		       TypeOfError:Exception ->
			   list_to_binary(io_lib:format("~p : ~p", [TypeOfError, Exception]))
		   end;
	       {error, enoent} ->
		   list_to_binary(io_lib:format("File not found: ~s~n", [Filename]));
	       {error, ReadErrReason} ->
		   list_to_binary(io_lib:format("Error: ~p~n", [ReadErrReason]))
	   end,

    %% io:format("DEBUG>>> php_handler:init Body ~p~n", [Body]),
    
    Resp = cowboy_req:reply(200, 
			    #{<<"content-type">> => <<"text/html">>}, 
			    Body, 
			    Req),
    
    {ok, Resp, OptsMap}.
