-module(billy_index_handler).
-export([init/2]).

init(Req0, Opts) ->
    Body = <<"<h1>BILLY IS HERE! BILLY WANT YOU MONEY!</h1>">>,
    Req = cowboy_req:reply(200, 
			   #{<<"content-type">> => <<"text/html">>}, 
			   Body, 
			   Req0),

    %% Req2 = cowboy_req:stream_reply(200, Req0),
    %% cowboy_req:stream_body("Hello\r\n", nofin, Req2),
    %% cowboy_req:stream_body("World 1\r\n", nofin, Req2),
    %% cowboy_req:stream_body("Chunked 2!\r\n", fin, Req2),
    
    {ok, Req, Opts}.
