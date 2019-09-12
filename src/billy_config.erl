-module(billy_config).
-author("Ilya Troshkov").
-export([get/1]).

%% [{k, v}]
get(#{merchant_config := MerchantConfig, key := Key}) when is_list(Key) ->
    ?MODULE:get(#{merchant_config => MerchantConfig, key => list_to_binary(Key)});
get(#{merchant_config := MerchantConfig, key := Key}) when is_binary(Key) ->
    case proplists:get_value(Key, MerchantConfig, undefined) of
	undefined -> {error, not_found};
	Val -> {ok, [{Key, Val}]}
    end;

%% [{k, v}]
get(#{merchant_id := MerchantId, key := Key}) ->
    case billy_mysql:exec_prepared_stmt(#{stmt => get_merchant_config_stmt, params => [MerchantId, Key]}) of
	{ok, []} -> {error, not_found};
	{ok, MerchantConfigRows} -> {ok, build_merchant_config(#{db_rows => MerchantConfigRows})}
    end;

%% [{k,v}, {k,v}, ...]
get(#{merchant_id := MerchantId}) ->
    case billy_mysql:exec_prepared_stmt(#{stmt => get_merchant_config_full_stmt, params => [MerchantId]}) of
	{ok, []} -> {error, not_found};
	{ok, MerchantConfigRows} -> {ok, build_merchant_config(#{db_rows => MerchantConfigRows})}
    end;

get(Par) ->
    case application:get_env(billy, Par) of
	{ok, Val} -> Val;
	undefined -> undefined
    end.




%% ======================================================================
%% INTERNAL FUNCTIONS


%% Функция преобразовывает конфигурацию мерчанта в {k, v} формат
%% [[],[],...[]] -> [{},{},...{}]
build_merchant_config(#{db_rows := DbRows}) ->
    lists:map(fun(DbRow) -> 
		      Key = proplists:get_value(<<"key">>, DbRow),
		      Val = proplists:get_value(<<"value">>, DbRow),
		      {Key, Val}
	      end, DbRows).








