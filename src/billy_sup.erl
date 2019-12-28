-module(billy_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(billy_sup, []).

init(_Args) ->
    {ok, {{one_for_one, 100, 1},
	  [
	   {billy_ipn_srv,          {billy_ipn_srv, start_link, []}, permanent, brutal_kill, worker, []},

	   {billy_cbr_rates_srv,    {billy_cbr_rates_srv, start_link, []}, permanent, brutal_kill, worker, []},

	   {billy_mp_srv_qiwi,      {billy_mp_srv, start_link, [#{method => qiwi}]}, permanent, brutal_kill, worker, []},
	   {billy_mp_srv_steam,     {billy_mp_srv, start_link, [#{method => steam}]}, permanent, brutal_kill, worker, []},
	   {billy_mp_srv_payeer,    {billy_mp_srv, start_link, [#{method => payeer}]}, permanent, brutal_kill, worker, []},
	   {billy_mp_srv_btc,       {billy_mp_srv, start_link, [#{method => btc}]}, permanent, brutal_kill, worker, []}

	   %% {billy_mp_srv_paypal,  {billy_mp_srv, start_link, [#{method => paypal}]}, permanent, brutal_kill, worker, []},
	   %% {billy_mp_srv_bitcoin, {billy_mp_srv, start_link, [#{method => bitcoin}]}, permanent, brutal_kill, worker, []}
	  ]
	 }
    }.
