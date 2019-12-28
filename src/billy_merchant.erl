-module(billy_merchant).

-export([get/1]).

%%%===================================================================
%%% Main module API
%%%===================================================================

get(ParamsMap) -> billy_merchant_db:get(ParamsMap).
