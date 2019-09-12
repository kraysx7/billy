-module(billy_testphp_handler).
-export([init/2]).

-include("../include/ephp.hrl").

init(Req0, Opts) ->

    Filename = "../priv/ephp/test_page.php",

    
    %% Читаем php файл
    Body = case file:read_file(Filename) of
	       {ok, Content} ->
		   %% AbsFilename = list_to_binary(filename:absname(Filename)),
		   {ok, Ctx} = ephp:context_new(),
		   {ok, Output} = ephp_output:start_link(Ctx, false),
		   ephp_context:set_output_handler(Ctx, Output),

		   %% SuperGlobals = [
		   %% 		   <<"_GET">>,
		   %% 		   <<"_POST">>,
		   %% 		   <<"_FILES">>,
		   %% 		   <<"_COOKIE">>,
		   %% 		   <<"_SESSION">>,
		   %% 		   <<"_REQUEST">>,
		   %% 		   <<"_ENV">>
		   %% 		  ],
		   %% lists:foreach(fun(Global) ->
		   %% 			 ephp_context:set(Ctx, #variable{name = <<"_POST">>}, ephp_array:new())
		   %% 		 end, SuperGlobals),
		   
		   PostPhpArray = ephp_array:from_list([
							{<<"test_param">>, <<"12345">>},
							{<<"signature">>, <<"2342vASD24fwervce45r2$#Q@%^@$%@#">>}
						       ]),

		   ephp_context:set(Ctx, #variable{name = <<"_POST">>}, PostPhpArray),

		   case ephp:eval(Ctx, Content) of
		       {ok, Return} ->
			   Result0 = ephp_context:get_output(Ctx),
			   Result = io_lib:format("~ts", [Result0]), 
			   unicode:characters_to_binary(Result, utf8);
		       {error, EvalErrReason, _Index, _File, _Level, _Data} ->
			   list_to_binary(io_lib:format("Process php error: ~p~n", [EvalErrReason]))
		   end;
	       {error, enoent} ->
		   list_to_binary(io_lib:format("File not found: ~s~n", [Filename]));
	       {error, ReadErrReason} ->
		   list_to_binary(io_lib:format("Error: ~p~n", [ReadErrReason]))
	   end,

    io:format("DEBUG>>> testphp_handler:init Body ~p~n", [Body]),

    Req = cowboy_req:reply(200, 
			   #{<<"content-type">> => <<"text/html">>}, 
			   Body, 
			   Req0),

    {ok, Req, Opts}.
