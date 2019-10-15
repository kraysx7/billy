-module(billy_config).
-author("Ilya Troshkov").
-export([get/1]).

%% @doc Получает настройки платёжной системы по id мерчанта и ключу системы
get(#{merchant_id := MerchantId, paysystem_key := PaySystemKey}) when is_list(PaySystemKey) ->
    ?MODULE:get(#{merchant_id => MerchantId, paysystem_key => list_to_binary(PaySystemKey)});
get(#{merchant_id := MerchantId, paysystem_key := PaySystemKey}) when is_binary(PaySystemKey) ->
    <<PaySystemId:32, _/binary>> = PaySystemKey,
    case billy_merchant_db:get_merchant_paysystem(#{merchant_id => MerchantId, paysystem_id => PaySystemId}) of
	{ok, []} -> {error, not_found};
	{ok, [MerchantPaySystem | _]} -> 
	    ConfigStr = proplists:get_value(<<"config">>, MerchantPaySystem),
	    ConfigMap = jiffy:decode(ConfigStr, [return_maps]),
	    {ok, ConfigMap}
    end;


%% @doc Получает конфиг мерчанта по заданному ключу
get(#{merchant_config := MerchantConfig, key := Key}) when is_list(Key) ->
    ?MODULE:get(#{merchant_config => MerchantConfig, key => list_to_binary(Key)});
get(#{merchant_config := MerchantConfig, key := Key}) when is_binary(Key) ->
    case proplists:get_value(Key, MerchantConfig, undefined) of
	undefined -> {error, not_found};
	Val -> {ok, [{Key, Val}]}
    end;

%% @doc Получает конфиг мерчанта по заданному ключу
get(#{merchant_id := MerchantId, key := Key}) ->
    case billy_merchant_db:get_config(#{merchant_id => MerchantId, key => Key}) of
	{ok, []} -> {error, not_found};
	{ok, MerchantConfigRows} -> {ok, build_merchant_config(#{db_rows => MerchantConfigRows})}
    end;

%% @doc Получает весь конфиг мерчанта
get(#{merchant_id := MerchantId}) ->
    case billy_merchant_db:get_config(#{merchant_id => MerchantId}) of
	{ok, []} -> {error, not_found};
	{ok, MerchantConfigRows} -> {ok, build_merchant_config(#{db_rows => MerchantConfigRows})}
    end;

get(Par) ->
    case application:get_env(billy, Par) of
	{ok, Val} -> Val;
	undefined -> undefined
    end.




%% ======================================================================
%% STUFF FUNCTIONS
%% ======================================================================

%% Функция преобразовывает конфигурацию мерчанта в {k, v} формат
%% [[],[],...[]] -> [{},{},...{}]
build_merchant_config(#{db_rows := DbRows}) ->
    lists:map(fun(DbRow) -> 
		      Key = proplists:get_value(<<"key">>, DbRow),
		      Val = proplists:get_value(<<"value">>, DbRow),
		      {Key, Val}
	      end, DbRows).








