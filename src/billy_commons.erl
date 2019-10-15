-module(billy_commons).
-compile([export_all]).

%% ----------------------------------------------------------

decode_erl_term(ErlTermStr) when is_list(ErlTermStr),length(ErlTermStr) > 0 ->
    {ok, T, _} = erl_scan:string(ErlTermStr),
    try erl_parse:parse_term(T) of
	{ok, ErlTerm} -> ErlTerm;
	{error, Reason} ->
	    io:format("DEBUG>>> billy_commons:decode_erl_term decode_erl_term ERROR: ~p~n", [Reason]),
	    ErlTermStr
    catch
	Error ->
	    io:format("DEBUG>>> billy_commons:decode_erl_term catch: ~p~n", [Error]),
	    ErlTermStr
    end;
decode_erl_term(ErlTermBinStr) when is_binary(ErlTermBinStr),byte_size(ErlTermBinStr) > 0 ->
    decode_erl_term(binary_to_list(ErlTermBinStr)).



list_to_integer(List) when is_list(List) ->
    case re:run(List, "^-?[0-9]*$") of
	{match, _} -> erlang:list_to_integer(List);
	nomatch -> badarg	  
    end;
list_to_integer(_) -> badarg.



list_to_float(List) when is_list(List)->
    case io_lib:fread("~f", List) of
	{ok, [Float], []} -> Float;
	_ ->
	    case io_lib:fread("~d", List) of
		{ok, [Int], []} -> float(Int);
		_ -> badarg
	    end
    end;
list_to_float(_) -> badarg.



safe_list_to_binary(undefined) ->
    <<"undef">>;
safe_list_to_binary(List) when is_list(List) ->
    list_to_binary(List);
safe_list_to_binary(Param) when is_atom(Param) ->
    erlang:atom_to_binary(Param, latin1);
safe_list_to_binary(List) when is_binary(List) -> List;
safe_list_to_binary(Param) ->
    io:format("DEBUG PARAM: ~p~n", [Param]),
    <<"undef">>.	


get_bin_from_string(String) when is_list(String)->
    unicode:characters_to_binary(String, utf8).



get_random_element([]) -> null;
get_random_element(ElementsList) ->
    %% {A1,A2,A3} = now(),
    %% random:seed(A1, A2, A3),
    Index = random:uniform(length(ElementsList)),
    lists:nth(Index, ElementsList).


%% ----------------------------------------------------------
%% Делаем перую букву заглавной
capfirst([Head | Tail]) when Head >= $a, Head =< $z ->
    [Head + ($A - $a) | Tail];
capfirst(Other) ->
    Other.



%% ----------------------------------------------------------
%% Функции работы с временем

%% Функция получает UTC время в секундах без смещения по часовому поясу
get_now_utc(sec) ->
    {Mega0, Sec0, Micro0} = erlang:timestamp(),
    CurDateTs = round((Mega0 * 1000000 * 1000000 + Sec0 * 1000000 + Micro0) / 1000000), %% NOW Timestamp in secs,
    CurDateTs.

%% Функция преобразовывает UTC время(в сек) в DateTime формат
utc_to_datetime(UtcSeconds) ->
   BaseDate      = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
   Seconds       = BaseDate + UtcSeconds,
   {Date,Time} = calendar:gregorian_seconds_to_datetime(Seconds),
   {Date,Time}.

%% Функция преобразовывает время в DateTime формате в UTC время(в сек)
-define(UNIX_EPOCH_MAGIC, 62167219200).
datetime_to_utc(DateTime) ->
    UtcSeconds = case DateTime of
		     undefined -> 0;
		     {{_,_,_},{_,_,_}} = DT ->
			 calendar:datetime_to_gregorian_seconds(DT)-?UNIX_EPOCH_MAGIC;
		     {datetime, {{_,_,_},{_,_,_}} = DT2} ->
			 calendar:datetime_to_gregorian_seconds(DT2)-?UNIX_EPOCH_MAGIC
		 end,
    UtcSeconds.
%% ----------------------------------------------------------



%% ----------------------------------------------------------
hex_md5(A) ->
    lists:flatten([io_lib:format("~2.16.0b", [N]) || N <- binary_to_list(erlang:md5(A))]).




%% ----------------------------------------------------------
%% Группировка проплиста по любому параметру, через функцию задающей ключ группировки
group_by(F, L) -> lists:foldr(fun({K,V}, D) -> dict:append(K, V, D) end, dict:new(), [ {F(X), X} || X <- L ]).




%% QuickSort
qsort([]) -> [];
qsort([Pivot|Rest]) ->
    qsort([ X || X <- Rest, X < Pivot]) ++ [Pivot] ++ qsort([ Y || Y <- Rest, Y >= Pivot]).


%% Ф-я вставляет разделитель между каждыми двумя элементами в списке
implode(_S, [])->
	[<<>>];
implode(S, L) when is_list(L) ->
    implode(S, L, []).
implode(_S, [H], NList) ->
    lists:reverse([thing_to_list(H) | NList]);
implode(S, [H | T], NList) ->
    L = [thing_to_list(H) | NList],
    implode(S, T, [S | L]).

%% Ф-я разбивает строку на колонки через разделитель
explode(S, B)->
    re:split(B, S, [{return, list}]).
explode(S, B, int) ->
    [erlang:list_to_integer(Str) || Str <- explode(S, B), length(Str) > 0].

thing_to_list(X) when is_integer(X) -> integer_to_list(X);
thing_to_list(X) when is_float(X)   -> float_to_list(X);
thing_to_list(X) when is_atom(X)    -> atom_to_list(X);
thing_to_list(X) when is_binary(X)  -> binary_to_list(X);
thing_to_list(X) when is_list(X) -> X.



%% ----------------------------------------------------------
upgrade_proplist(Proplist, UpProplist) ->
    NewProplist = lists:foldl(fun({Key, Val}, R) ->
				      case Val of
					  delete -> 
					      proplists:delete(Key, R);
					  Val ->
					      R0 = proplists:delete(Key, R),
					      lists:keystore(Key, 1, R0, {Key, Val})	       
				      end
			      end, Proplist, UpProplist),
    NewProplist.


%% ----------------------------------------------------------
%% Форматирование стоимости предметов (или баланс и пр что связано с деньгами)
%% Переводит центы в формат xx.yy , где yy - обрезается по значению вместе с нулями
format_ui_price(PriceInCents) ->
    UiPrice = case PriceInCents rem 100 of
		  0 -> 
		      list_to_binary(float_to_list(PriceInCents / 100, [{decimals,0}, compact]));
		  DP when DP =< 10 ->
		      list_to_binary(float_to_list(PriceInCents / 100, [{decimals,1}, compact]));
		  DP when DP > 10 ->
		      list_to_binary(float_to_list(PriceInCents / 100, [{decimals,2}, compact]))
	      end,
    UiPrice.



%% Функция переводит 3х буквенный код валюты в числовой. пока костыль..
get_currency_number(#{currency_alpha := CcyAlpha}) when is_list(CcyAlpha) -> 
    get_currency_number(#{currency_alpha => list_to_binary(CcyAlpha)});
get_currency_number(#{currency_alpha := CcyAlpha}) when is_binary(CcyAlpha) ->
    case CcyAlpha of
	<<"RUB">> -> 643;
	<<"USD">> -> 840;
	<<"EUR">> -> 978;
	<<"BTC">> -> 886684 %% "XBT" = [[88,66,84]]
    end.


get_billy_signature(#{bill_id := BillId, amount := Amount, ccy := Ccy, merchant_secret_key := MerchantSecretKey}) ->    
    SignStr = io_lib:format("~p~p~ts", [BillId, Amount, Ccy]),
    Sign = crypto:hmac(sha256, MerchantSecretKey, SignStr),
    lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= Sign]).



make_doc_xml(XmlFields, Prolog) ->
    Xml = xmerl:export_simple([XmlFields], xmerl_xml, [{prolog, Prolog}]),
    unicode:characters_to_binary(Xml).
