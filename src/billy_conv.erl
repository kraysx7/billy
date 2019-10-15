%%% @doc conversion routines between any types and common formats
%%% @end
%%% @private
-module(billy_conv).

%% Conversion routines
-export([
         record_as_proplist/2,
         proplist_as_record/3
	]).

%% @doc Преобразовывает рекорд в проплист с соответствующими полями
record_as_proplist(Rec, Fields) when is_list(Fields) ->
    Keys = [ atom_to_binary(F, latin1) || F <- Fields ],
    lists:zip(Keys, tl(tuple_to_list(Rec))).

%% @doc Преобразовывает проплист в рекорд с соответствующими полями
proplist_as_record(Proplist, RecordName, Fields) when is_list(Proplist), is_atom(RecordName), is_list(Fields) ->
    %% Распаковываем проплист на ключи и значения
    {Keys, Values} = lists:unzip(Proplist),
    
    %% Создаём секвенцию чисел-индексов для ключей проплиста
    S = lists:seq(1, length(Keys)),
    
    %% Объединяем список ключей с индексами
    P = lists:zip([ binary_to_atom(K, utf8) || K <- Keys ], S),

    %% Определяем функцию, которая возвращает другую функцию, 
    %% которая возвращает значение из проплиста по номеру в секвенции
    F = fun(FieldName) ->
		case proplists:lookup(FieldName, P) of
		    none ->
			fun(_) -> undefined end;
		    {FieldName, Pos} ->
			fun(Row) -> lists:nth(Pos, Row) end
		end
	end,
    Fs = [ F(FieldName) || FieldName <- Fields ],
    F1 = fun(Row) ->
		 RecordData = [ Fx(Row) || Fx <- Fs ],
		 list_to_tuple([RecordName|RecordData])
	 end,
    F1(Values).
