-module(billy_notfound_handler).
-export([init/2]).

init(Req0, Opts) ->
    Body = <<"<h1>404 Page Not Found</h1>">>,
    Req = cowboy_req:reply(404, 
			   #{<<"content-type">> => <<"text/html">>},
			   Body, 
			   Req0),
    {ok, Req, Opts}.
