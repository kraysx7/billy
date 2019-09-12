-module(billy_query_helper).
-compile([export_all]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Функции проверки типов

finaly_check(CheckResult) ->
    lists:foldl(fun(E, R) -> 
			case R of
			    false -> false;
			    true ->
				case E of
				    {_, badarg} -> false;
				    {_, _} -> true
				end
			end
		end, true, CheckResult).


check_integer([]) -> badarg;
check_integer(SInt) when is_integer(SInt) -> SInt;
check_integer(SInt) when is_binary(SInt) -> check_integer(binary_to_list(SInt));
check_integer(SInt) when is_list(SInt) ->
    case billy_commons:list_to_integer(SInt) of
	badarg -> badarg;
	Int -> Int
    end;
check_integer(_SInt) -> badarg.



check_float(SFloat) when is_binary(SFloat) -> check_float(binary_to_list(SFloat));
check_float(SFloat) when is_list(SFloat)->
    case billy_commons:list_to_float(SFloat) of
	badarg -> badarg;
	Int -> Int
    end;
check_float(_) -> badarg.



check_atom([]) -> badarg;
check_atom(SAtom) when is_binary(SAtom) -> check_atom(binary_to_list(SAtom));
check_atom(SAtom) when is_list(SAtom) ->
    case erlang:list_to_atom(SAtom) of
	badarg -> badarg;
	Atom -> Atom
    end;
check_atom(_SAtom) -> badarg.



check_string(Str) when is_binary(Str) -> check_string(binary_to_list(Str));
check_string(Str) when is_list(Str) -> Str;
check_string(_) -> badarg.
check_string([], _) -> [];
check_string(Str, MaxLen) when is_binary(Str) -> check_string(binary_to_list(Str), MaxLen);
check_string(Str, MaxLen) when is_list(Str),length(Str) =< MaxLen -> Str;
check_string(_, _) -> badarg.


check_binary_string(Str) when is_binary(Str) -> Str;
check_binary_string(Str) when is_list(Str) -> list_to_binary(Str);
check_binary_string(_) -> badarg.


check_regexp([], _RegEx) -> badarg;
check_regexp(undefined, _RegEx) -> badarg;
check_regexp(Str, RegExp) when is_list(Str) ->
    case re:run(Str, RegExp) of
	{match, _} -> Str;
	nomatch -> badarg
    end;
check_regexp(_,_) -> badarg.



check_group(ParamsList, string) ->
    CheckGrpRes = lists:foldl(fun(P, R) -> 
				      case R of
					  badarg -> badarg;
					  true ->
					      CheckRes = billy_query_helper:check_string(P, 32),
					      case CheckRes of
						  badarg -> badarg;
						  _ -> true
					      end
				      end
			      end, true, ParamsList),
    case CheckGrpRes of
	badarg -> badarg;
	true -> ParamsList
    end;

check_group(ParamsList, int) ->
    CheckGrpRes = lists:foldl(fun(P, R) -> 
				      case R of
					  badarg -> badarg;
					  true ->
					      CheckRes = billy_query_helper:check_integer(P),
					      case CheckRes of
						  badarg -> badarg;
						  _ -> true
					      end
				      end
			      end, true, ParamsList),
    case CheckGrpRes of
	badarg -> badarg;
	true -> ParamsList
    end.
