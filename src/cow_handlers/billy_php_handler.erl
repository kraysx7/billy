-module(billy_php_handler).

-export([init/2]).

-include("ephp.hrl").

init(Req, OptsMap) ->

    error_logger:info_msg("DEBUG>>> billy_php_handler:init Req = ~p~n", [Req]),

    #{php_file := PhpFile} = OptsMap,
    Filename = io_lib:format("priv/ephp/web_controller/~ts", [PhpFile]),
    
    {ok, ReqParamsKV, _} = cowboy_req:read_urlencoded_body(Req),
    EPhpPostVar = ephp_array:from_list(ReqParamsKV),

    QsParamsKV = cowboy_req:parse_qs(Req),
    EPhpGetVar = ephp_array:from_list(QsParamsKV),

    EvalPhpRes = eval_php_file(#{filename => Filename, ephp_post_var => EPhpPostVar, ephp_get_var => EPhpGetVar}), 
    
    Resp = case EvalPhpRes of 
	       %% Ответ без заголовков
	       {ok, undefined, Output} ->
		   DefaultHeaders_200 = #{<<"content-type">> => <<"text/plain">>},
		   cowboy_req:reply(200, DefaultHeaders_200, Output, Req);

	       %% Ответ c заголовками
	       {ok, Headers, Output} ->

		   RespCode = case maps:get(<<"location">>, Headers, undefined) of
				  undefined -> 200;
				  _ -> 302
			      end,
		   
		   cowboy_req:reply(RespCode, Headers, Output, Req);

	       %% Ошибка во время выполнения
	       {error, eval_php_error, ErrorMessage} ->
		   cowboy_req:reply(500, #{}, ErrorMessage, Req);

	       %% Синтаксическая ошибка
	       {catch_error, _, _} -> 
		   cowboy_req:reply(500, Req);
	       
	       %% php файл не найден
	       {error, enoent} ->
		   ErrorMessage = io_lib:format("404: File Not Found > ~ts", [Filename]),
		   cowboy_req:reply(404, #{}, ErrorMessage, Req);

	       %% Какая то другая ошибка при чтении файла
	       {error, ErrorReason} ->
		   ErrorMessage = io_lib:format("500: Read File Error > ~p", [ErrorReason]),
		   cowboy_req:reply(500, #{}, ErrorMessage, Req);

	       %% Просто 500
	       _ ->
		   cowboy_req:reply(500, Req)
	   end,

    {ok, Resp, OptsMap}.


eval_php_file(#{filename := Filename, ephp_post_var := EPhpPostVar, ephp_get_var := EPhpGetVar}) ->
    %% Читаем php файл
    case file:read_file(Filename) of
	{ok, PHP} ->

	    %% Создаём контекст
	    {ok, Ctx} = ephp:context_new(),
	    ephp:register_module(Ctx, billy_ephp_lib),

	    {ok, OutputHandler} = ephp_output:start_link(Ctx, false),
	    ephp_context:set_output_handler(Ctx, OutputHandler),

	    %% Настраиваем переменные окружения
	    ephp_context:set(Ctx, #variable{name = <<"_POST">>}, EPhpPostVar),
	    ephp_context:set(Ctx, #variable{name = <<"_GET">>}, EPhpGetVar),

	    %% Выполняем загруженный php код
	    try ephp:eval(Filename, Ctx, PHP) of
		{ok, _Return} ->
		    %% Получить данные на выходе
		    Output = ephp_context:get_output(Ctx),
		    %% OutputBin = unicode:characters_to_binary(Result, utf8),
		    
		    Headers = ephp_context:get_meta(Ctx, <<"headers">>),

		    {ok, Headers, Output};

		{error, EvalErrReason, Index, File, Level, Data} = Error ->
		    ErrorMessage0 = ephp_context:get_output(Ctx),
		    ErrorMessage = unicode:characters_to_binary(ErrorMessage0, utf8),
		    lager:error("php eval error: ~ts", [ErrorMessage]),
		    {error, eval_php_error, ErrorMessage}
	    catch
		TypeOfError:Exception ->
		    lager:error("php eval catch: ~p ; ex: ~p~n", [TypeOfError, Exception]),
		    {catch_error, TypeOfError, Exception}
	    end;
	{error, enoent} ->
	    lager:error("file not found: ~ts~n", [Filename]),
	    {error, enoent};
	{error, ReadErrReason} ->
	    lager:error("read file error: ~ts ; reason: ~ts~n", [Filename, ReadErrReason]),
	    {error, ReadErrReason}
    end.
