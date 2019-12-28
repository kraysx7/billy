-module(billy_ephp_lib).
-compile([warnings_as_errors]).

-include("ephp.hrl").

-behaviour(ephp_func).

-export([
	 init_func/0,
	 init_config/0,
	 init_const/0,
	 reload_routes/2,

	 header/4,

	 get_config/3,
	 get_merchant_config/4,
	 get_paysystem_config/4,

	 get_merchant/3,
	 get_transaction/3,

	 update_transaction_param/5,
	 process_transaction/4,
	 check_billy_signature/4,

	 float_to_string/4,
	 format_str/4,
	 characters_to_binary/5,

	 sort/3,
	 implode/4,
	 base64_decode/3,
	 base64_encode/3,
	 hash/5,
	 hash/4,
	 hash_hmac/5,

	 number_format/4,

	 http_uri_encode/3,
	 curl/7,
	 json_decode/3,
	 json_encode/3
	]).



-spec init_func() -> ephp_func:php_function_results().

init_func() -> [
		reload_routes,

		{header, [{args, [string, string]}]},

		{get_config, [{args, [string]}]},
		{get_merchant_config, [{args, [int, string]}]},
		{get_paysystem_config, [{args, [int, string]}]},

		{get_merchant, [{args, [int]}]},
		{get_transaction, [{args, [int]}]},

		{update_transaction_param, [{args, [int, string, string]}]},
		{update_transaction_param, [{args, [int, string, int]}]},

		{process_transaction, [{args, [int, string]}]},
		
		{check_billy_signature, [{args, [int, string]}]},

		{float_to_string, [{args, [float, int]}]},
		{format_str, [{args, [string, array]}]},
		{characters_to_binary, [{args, [string, string, string]}]},
		
		{sort, [{args, [array]}]},
		{implode, [{args, [string, array]}]},
		{base64_decode, [{args, [string]}]},
		{base64_encode, [{args, [string]}]},
		{hash, [{args, [string, string, boolean]}]},
		{hash, [{args, [string, string]}]},
		{hash_hmac, [{args, [string, string, string]}]},

		{number_format, [{args, [float, int]}]},

		{http_uri_encode, [{args, [string]}]},
		{curl, [{args, [string, string, array, string, array]}]},
		{json_decode, [{args, [string]}]},
		{json_encode, [{args, [array]}]}
	       ].



-spec init_config() -> ephp_func:php_config_results().

init_config() -> [].


-spec init_const() -> ephp_func:php_const_results().

init_const() -> [].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BulidIn Billy PHP Functions

reload_routes(_Context, _Line) ->
    billy_cow_routes:reload_routes().


header(Context, _Line, {_, HeaderKey}, {_, HeaderVal}) ->
    case ephp_context:get_meta(Context, <<"headers">>) of
	undefined ->
	    ephp_context:set_meta(Context, <<"headers">>, #{HeaderKey => HeaderVal});
	Headers ->
	    NewHeaders = maps:merge(Headers, #{HeaderKey => HeaderVal}),
	    ephp_context:set_meta(Context, <<"headers">>, NewHeaders)
    end.
    


get_config(_Context, _Line, {_, ConfigKey}) ->
    billy_config:get(binary_to_atom(ConfigKey, utf8)).


get_merchant_config(_Context, _Line, {_, MerchantId}, {_, ConfigKey}) ->
    {ok, MerchantConfig} = billy_config:get(#{merchant_id => MerchantId}),
    {ok, [{_, ConfigValue}]} = billy_config:get(#{merchant_config => MerchantConfig, key => ConfigKey}),
    ConfigValue.


get_paysystem_config(_Context, _Line, {_, MerchantId}, {_, PaySystemKey}) ->
    case billy_config:get(#{merchant_id => MerchantId, paysystem_key => PaySystemKey}) of
	{ok, ConfigMap} -> 
	    %% Т.к. api возвращает map, делаем преобразование в erljson таким радикальным способом
	    %% TODO: дописать функцию -> map_to_ephp_array()
	    ConfigErlJson = jiffy:decode(jiffy:encode(ConfigMap)),
	    erljson_to_ephp_array(#{erljson => ConfigErlJson});
	{error, not_found} -> proplist_to_ephp_array(#{proplist => []})
    end.


get_merchant(_Context, _Line, {_, MerchantId}) ->
    case billy_merchant:get(#{merchant_id => MerchantId}) of
	%% Такого мерчанта не существует 
	{ok, []} -> proplist_to_ephp_array(#{proplist => []});

	%% Мерчант успешно получен
	{ok, [Merchant | _]} -> proplist_to_ephp_array(#{proplist => Merchant});

	%% Такого мерчанта не существует 
	{error, not_found} -> proplist_to_ephp_array(#{proplist => []})
    end.


get_transaction(_Context, _Line, {_, TrId}) ->
    case billy_transaction:get(#{transaction_id => TrId}) of
	%% Такой транзакции не существует
	{ok, []} -> proplist_to_ephp_array(#{proplist => []});

	%% Транзакция успешно получена
	{ok, [Tr | _]} -> proplist_to_ephp_array(#{proplist => Tr});

	%% Такой транзакции не существует
	{error, not_found} -> proplist_to_ephp_array(#{proplist => []})
    end.


update_transaction_param(_Context, _Line, {_, TrId}, {_, ParamKey}, {_, ParamValue}) ->
    case billy_transaction:get(#{transaction_id => TrId}) of
	%% Транзакция успешно получена
	{ok, [Tr | _]} ->
					 
	    %% Сохранить токен в параметрах транзакции
	    TrParamsBin = proplists:get_value(<<"params">>, Tr),
	    TrParams = jiffy:decode(TrParamsBin, [return_maps]),
	    NewTrParams = maps:merge(TrParams, #{ParamKey => ParamValue}),
	    NewTrParamsBin = jiffy:encode(NewTrParams),
	    
	    %% io:format("DEBUG>>> billy_ephp_lib:update_transaction_param #~p  NewTrParamsBin: ~ts~n", [TrId, NewTrParamsBin]),
	    
	    case billy_transaction:update(#{transaction_id => TrId, params => NewTrParamsBin}) of
		%% Параметры успешно обновлены
		ok -> 0;

		%% Произошла ошибка во время работы с бд
		_ -> 2 
	    end;

	%% Такой транзакции не существует
	{error, not_found} -> 1 
    end.



process_transaction(_Context, _Line,  {_, TrId}, {_, ProcessResult}) ->
    %% ProcessResult = <<"success">> | <<"fail">> | <<"refund">> | <<"...">>,
    case billy_payment:process_transaction(#{transaction_id => TrId, process_result => ProcessResult}) of
	%% Транзакция успешно обработана
	ok ->

	    %% Немедленно вызвать IPN к мерчанту
	    NotifyParamsMap = #{transaction_id => TrId},
	    wpool:cast(billy_ipn_wpool, {notify, NotifyParamsMap}),	    
	    0;

	%% Транзакция уже обработана
	{error, transaction_already_processed} -> 1;

	%% Произошла ошибка во время работы с бд
	{error, _Reason} -> 2
    end.



check_billy_signature(_Context, _Line, {_, TrId}, {_, Signature}) ->
    %% Получить транзакцию
    case billy_transaction:get(#{transaction_id => TrId}) of
	{ok, [TrProplist | _]} ->
	    %% Получить мерчанта		    
	    MerchantId = proplists:get_value(<<"merchant_id">>, TrProplist),

	    %% Загружаем конфиг мерчанта
	    {ok, MerchantConfig} = billy_config:get(#{merchant_id => MerchantId}),
	    
	    %% Получаем секретный ключ мерчанта
	    {ok, [{_, MerchantSecretKey}]} = billy_config:get(#{merchant_config => MerchantConfig, key => "secret_key"}),

	    %% Формируем сигнатуру
	    TrParamsBinJson = proplists:get_value(<<"params">>, TrProplist),
	    TrParams = jiffy:decode(TrParamsBinJson, [return_maps]),
	    
	    BillId = maps:get(<<"bill_id">>, TrParams),
	    Amount = proplists:get_value(<<"amount">>, TrProplist),
	    Ccy = proplists:get_value(<<"ccy_alpha">>, TrProplist),
	    
	    BillySignParams = #{bill_id => BillId, amount => Amount, ccy => Ccy, merchant_secret_key => MerchantSecretKey},
	    BillySign = list_to_binary(billy_commons:get_billy_signature(BillySignParams)),

	    %% Проверить сигнатуру
	    case Signature of
		%% Сигнатура в порядке
		BillySign -> true;
		_ ->  false
	    end;
	_ -> false
    end.




float_to_string(_Context, _Line,  {_, Float}, {_, Decimals}) ->
    list_to_binary(float_to_list(Float, [{decimals, Decimals}])).


format_str(_Context, _Line,  {_, FormatStr}, {_, FormatParamsArray}) ->
    FormatParamsList = ephp_array_to_list(#{ephp_array => FormatParamsArray}),
    unicode:characters_to_binary(io_lib:format(FormatStr, FormatParamsList), utf8).

characters_to_binary(_Context, _Line,  {_, Data}, {_, InEncoding}, {_, OutEncoding}) ->
    unicode:characters_to_binary(binary_to_list(Data), binary_to_atom(InEncoding, utf8), binary_to_atom(OutEncoding, utf8)).


sort(_Context, _Line,  {_, Array}) ->
    ArrayList = ephp_array_to_list(#{ephp_array => Array, options => [values_to_string]}),
    SortedArrayList = billy_commons:qsort(ArrayList),
    list_to_ephp_array(#{list => SortedArrayList}).



implode(_Context, _Line,  {_, String}, {_, Array}) ->
    ArrayList = ephp_array_to_list(#{ephp_array => Array, options => [values_to_string]}),
    R = lists:flatten(io_lib:format("~ts", [billy_commons:implode(String, ArrayList)])), 
    list_to_binary(R).



base64_decode(_Context, _Line,  {_, String}) ->
    base64:decode(String).

base64_encode(_Context, _Line,  {_, String}) ->
    base64:encode(String).



hash(_Context, _Line,  {_, Type}, {_, String}, {_, RawOutput}) ->
    RawCryptoData = crypto:hash(binary_to_atom(Type, utf8), String),
    case RawOutput of
	true -> RawCryptoData;
	false -> 
	    list_to_binary(
	      lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= RawCryptoData])
	     )
    end.
hash(Context, Line, PType, PString) ->
    hash(Context, Line, PType, PString, {123, false}).


hash_hmac(_Context, _Line,  {_, Type}, {_, Key}, {_, Data}) ->
    RawCryptoData = crypto:hmac(binary_to_atom(Type, utf8), Key, Data),
    list_to_binary(
      lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= RawCryptoData])
     ).


number_format(_Context, _Line,  {_, Float}, {_, Decimals}) ->
    Format = lists:flatten(io_lib:format("~ts~pf", ["~.", Decimals])),
    list_to_binary(io_lib:format(Format, [Float])).



http_uri_encode(_Context, _Line,  {_, Str}) ->
    list_to_binary(http_uri:encode(binary_to_list(Str))).

curl(_Context, _Line,  {_, Method}, {_, Url}, {_, HeadersEphpArray}, {_, Body}, {_, OptionsEphpArray}) ->

    {ephp_array, _Size, Headers, _, _} = HeadersEphpArray,

    Options = ephp_array_to_proplist(#{ephp_array => OptionsEphpArray, options => [binary_key_to_atom]}),

    %% io:format("DEBUG>>> billy_ephp_lib:curl Method: ~ts~nUrl=~ts~nHeaders=~p~nOptions=~p~nBody=~ts~n", [Method, Url, Headers, Options, Body]),

    case hackney:request(binary_to_atom(Method, utf8), Url, Headers, Body, Options) of
	%% Запрос выполнен
	{ok, HttpErrorCode, RespHeaders, ClientRef} ->
	    {ok, RespBody} = hackney:body(ClientRef),
	    Result = [
		      {<<"code">>, HttpErrorCode},
		      {<<"response">>, RespBody},
		      {<<"headers">>, RespHeaders}
		     ],
	    proplist_to_ephp_array(#{proplist => Result});
	%% Такого домена не существует (received an NXDOMAIN error from a DNS)
	{error, nxdomain} ->
	    Result = [{<<"code">>, 500},
		      {<<"response">>, <<"nxdomain">>}],
	    proplist_to_ephp_array(#{proplist => Result});
	%% Ошибка tls=недействительный сертификат. 
	{error, {tls_alert,"handshake failure"}} ->
	    Result = [{<<"code">>, 525},
		      {<<"response">>, <<"SSL Handshake Failed">>}],
	    proplist_to_ephp_array(#{proplist => Result});
	%% Костыль-заглушка для отладки. потом убрать
	_HackneyRequestRes ->
	    io:format("DEBUG>>> billy_ephp_lib:curl HackneyRequestRes=~p~n", [_HackneyRequestRes]),
	    Result = [{<<"code">>, <<"unknown">>},
		      {<<"response">>, <<"unknown">>}],
	    proplist_to_ephp_array(#{proplist => Result})
    end.

json_decode(_Context, _Line,  {_, JsonStr}) ->
    DecodedJsonStr = jiffy:decode(JsonStr),
    E = erljson_to_ephp_array(#{erljson => DecodedJsonStr}),
    %% io:format("DEBUG>>> billy_ephp_lib:json_decode E=~p~n", [E]),
    E.

json_encode(_Context, _Line,  {_, PhpArray}) ->
    ErlJson = ephp_array_to_erljson(#{ephp_array => PhpArray}),
    EncodedPhpArray = jiffy:encode(ErlJson),
    EncodedPhpArray.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Staff Functions

proplist_to_ephp_array(#{proplist := Proplist}) ->
    lists:foldl(fun
		    ({<<"params">> = K, V}, A) -> 
			{DV} = jiffy:decode(V),
			SubArray = proplist_to_ephp_array(#{proplist => DV}),
			ephp_array:store(K, SubArray, A);
		    ({K, V}, A) when is_binary(V) -> 
			ephp_array:store(K, V, A);
		    ({K, V}, A) when is_integer(V) -> 
			ephp_array:store(K, V, A);
		    ({K, V}, A) when is_float(V) -> 
			ephp_array:store(K, V, A);
		    ({K, V}, A) when is_atom(V)-> 
			VBin = list_to_binary(io_lib:format("~p", [V])),
			ephp_array:store(K, VBin, A);
		    ({K, {datetime, DT}} , A) -> 
			VBin = list_to_binary(io_lib:format("~p", [DT])),
			ephp_array:store(K, VBin, A);
		    ({K, {E}}, A) when is_list(E) -> 
			SubArray = proplist_to_ephp_array(#{proplist => E}),
			ephp_array:store(K, SubArray, A);
		    ({K, E}, A) when is_list(E) -> 
			SubArray = proplist_to_ephp_array(#{proplist => E}),
			ephp_array:store(K, SubArray, A)
		end, ephp_array:new(), Proplist).




erljson_to_ephp_array(#{erljson := ErlJson}) when is_tuple(ErlJson)  ->
    {ErlJsonProplist} = ErlJson,
    lists:foldl(fun
		    ({K, V}, A) when is_tuple(V) -> 
			SubArray = erljson_to_ephp_array(#{erljson => V}),
			ephp_array:store(K, SubArray, A);
		    ({K, V}, A) when is_list(V) -> 
			SubArray = lists:foldl(fun(E, A2) ->
						       V2 = erljson_to_ephp_array(#{erljson => E}),
						       ephp_array:store(auto, V2, A2)
					       end, ephp_array:new(), V),
			ephp_array:store(K, SubArray, A);
		    ({K, V}, A) when is_binary(V) -> 
			ephp_array:store(K, V, A);
		    ({K, V}, A) when is_integer(V) -> 
			ephp_array:store(K, V, A);
		    ({K, V}, A) when is_float(V) -> 
			ephp_array:store(K, V, A);
		    ({K, V}, A) when is_atom(V) -> 
			VBin = list_to_binary(io_lib:format("~p", [V])),
			ephp_array:store(K, VBin, A)
		end, ephp_array:new(), ErlJsonProplist).

ephp_array_to_erljson(#{ephp_array := EPhpArray}) ->
    {ephp_array, _Size, Elements, _, _} = EPhpArray,
    P = lists:foldl(fun 
			({K, V}, A) when ?IS_ARRAY(V) ->
			    SubArray = ephp_array_to_erljson(#{ephp_array => V}),
			    lists:append(A, [{K, SubArray}]);
			({K, V}, A) when is_atom(V) ->
			    VBin = list_to_binary(io_lib:format("~p", [V])),
			    lists:append(A, [{K, VBin}]);
			({K, V}, A)  ->
			    lists:append(A, [{K, V}])
		    end, [], Elements),
    {P}.


ephp_array_to_proplist(#{ephp_array := EPhpArray, options := [binary_key_to_atom]}) ->
    {ephp_array, _Size, Elements, _, _} = EPhpArray,
    P = lists:foldl(fun 
			({K, V}, A) when ?IS_ARRAY(V) ->
			    SubArray = ephp_array_to_proplist(#{ephp_array => V, options => [binary_key_to_atom]}),
			    lists:append(A, [{binary_to_atom(K, utf8), SubArray}]);
			({K, V}, A) ->
			    lists:append(A, [{binary_to_atom(K, utf8), V}])
		    end, [], Elements),
    P.


ephp_array_to_list(#{ephp_array := EPhpArray, options := [values_to_string]}) ->
    {ephp_array, _Size, Elements, _, _} = EPhpArray,
    P = lists:foldl(fun 
			({_K, V}, A) when ?IS_ARRAY(V) ->
			    SubArray = ephp_array_to_list(#{ephp_array => V}),
			    lists:append(A, [SubArray]);
			({_K, V}, A) ->
			    VStr = billy_commons:thing_to_list(V),
			    lists:append(A, [VStr])
		    end, [], Elements),
    P;


ephp_array_to_list(#{ephp_array := EPhpArray}) ->
    {ephp_array, _Size, Elements, _, _} = EPhpArray,
    P = lists:foldl(fun 
			({_K, V}, A) when ?IS_ARRAY(V) ->
			    SubArray = ephp_array_to_list(#{ephp_array => V}),
			    lists:append(A, [SubArray]);
			({_K, V}, A) ->
			    lists:append(A, [V])
		    end, [], Elements),
    P.


list_to_ephp_array(#{list := List}) ->
    lists:foldl(fun
		    (E, A) when is_binary(E) -> 
			ephp_array:store(auto, E, A);
		    (E, A) when is_integer(E) -> 
			ephp_array:store(auto, E, A);
		    (E, A) when is_float(E) -> 
			ephp_array:store(auto, E, A);
		    (E, A) when is_atom(E)-> 
			VBin = list_to_binary(io_lib:format("~p", [E])),
			ephp_array:store(auto, VBin, A);
		    ({datetime, DT}, A) -> 
			EBin = list_to_binary(io_lib:format("~p", [DT])),
			ephp_array:store(auto, EBin, A);
		    ({E}, A) when is_list(E) -> 
			SubArray = list_to_ephp_array(#{list => E}),
			ephp_array:store(auto, SubArray, A);
		    (E, A) when is_list(E) -> 
			SubArray = list_to_ephp_array(#{list => E}),
			ephp_array:store(auto, SubArray, A)
		end, ephp_array:new(), List).


%% lists:foldl(fun
%% 		    ({<<"params">> = K, V}, A) -> 
%% 			{DV} = jiffy:decode(V),
%% 			SubArray = proplist_to_ephp_array(#{proplist => DV}),
%% 			ephp_array:store(K, SubArray, A);
%% 		    ({K, V}, A) when is_binary(V) -> 
%% 			ephp_array:store(K, V, A);
%% 		    ({K, V}, A) when is_integer(V) -> 
%% 			ephp_array:store(K, V, A);
%% 		    ({K, V}, A) when is_atom(V)-> 
%% 			VBin = list_to_binary(io_lib:format("~p", [V])),
%% 			ephp_array:store(K, VBin, A);
%% 		    ({K, {datetime, DT}} , A) -> 
%% 			VBin = list_to_binary(io_lib:format("~p", [DT])),
%% 			ephp_array:store(K, VBin, A);
%% 		    ({K, {E}}, A) when is_list(E) -> 
%% 			SubArray = proplist_to_ephp_array(#{proplist => E}),
%% 			ephp_array:store(K, SubArray, A)
%% 		end, ephp_array:new(), Proplist).
