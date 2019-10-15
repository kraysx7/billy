-module(billy_cow_routes).
-export([load_routes/0, compile_dispatch/1, reload_routes/0]).


load_routes() ->
    %% Load  routes
    RoutesFile = "priv/billy.routes",
    case file:consult(RoutesFile) of
	{ok, LoadedTerms} ->
	    Routes = lists:foldl(fun
				     ({Url, Proplist}, ResRoutes) when is_list(Proplist) ->
					 Cntl = proplists:get_value(controller, Proplist),
					 
					 case Cntl of
					     Cntl when is_atom(Cntl) ->
						 Opts = proplists:get_value(opts, Proplist),
						 R = {Url, Cntl, Opts},
						 lists:append(ResRoutes,[R]);
					     Cntl when is_list(Cntl) ->
						 R = {Url, billy_php_handler, #{php_file => Cntl}},
						 lists:append(ResRoutes,[R])
					 end
				 end, [], LoadedTerms),
	    {ok, Routes};
	Error ->
	    Reason = io_lib:format("Missing or invalid billy.routes file in ~p; Error reason: ~p", [RoutesFile, Error]),
	    io:format(Reason),
	    {error, Reason}
    end.


compile_dispatch(Routes) ->
    cowboy_router:compile([
			   {'_', Routes}
			  ]).


reload_routes() ->
    {ok, Routes} = load_routes(),
    Dispatch = compile_dispatch(Routes),
    Name = my_http_listener,
    cowboy:set_env(Name, dispatch, Dispatch).
