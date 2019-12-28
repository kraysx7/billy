-module(billy_transaction).

-export([create/1, get/1, update/1, close/1]).

-include("../include/billy_transaction.hrl").

%%%===================================================================
%%% Main module API
%%%===================================================================

create(ParamsMap) -> billy_transaction_db:create(ParamsMap).



get(#{transaction_id := TrId, mode := cache}) ->
    ok = check_cache(#{transaction_id => TrId}),
    CacheTrRes = get_cache(#{transaction_id => TrId}),
    case CacheTrRes of    
	[CacheTrRec | _] when is_record(CacheTrRec, billy_transaction) ->
	    {ok, as_proplist(CacheTrRec)}
    end;

get(ParamsMap) -> billy_transaction_db:get(ParamsMap).



update(#{transaction_id := TrId, old_ipn_process_stage := OldIpnStage, new_ipn_process_stage := NewIpnStage, mode := cache}) ->
    ok = check_cache(#{transaction_id => TrId}),
    update_cache(#{transaction_id => TrId, old_ipn_process_stage => OldIpnStage, new_ipn_process_stage => NewIpnStage});

update(#{transaction_id := TrId, old_status := OldStatus, new_status := NewStatus, mode := cache}) ->
    ok = check_cache(#{transaction_id => TrId}),
    update_cache(#{transaction_id => TrId, old_status => OldStatus, new_status => NewStatus});

update(#{transaction_id := TrId, ipn_process_date := IpnProcessDate, mode := cache}) ->
    ok = check_cache(#{transaction_id => TrId}),
    update_cache(#{transaction_id => TrId, ipn_process_date => IpnProcessDate});

update(#{transaction_id := TrId, ipn_process_stage := IpnProcessStage, mode := cache}) ->
    ok = check_cache(#{transaction_id => TrId}),
    update_cache(#{transaction_id => TrId, ipn_process_stage => IpnProcessStage});

update(ParamsMap) -> billy_transaction_db:update(ParamsMap).



close(ParamsMap) -> billy_transaction_db:close(ParamsMap).



%% update(#{transaction_id := TrId, params := Params, params_type := json_str}) ->
%%     ParamsStr = lists:flatten(io_lib:format("~ts", [Params])),
%%     billy_transaction_db:update(#{transaction_id => TrId, params => ParamsStr});



%% update(#{transaction_id := TrId, params := Params}) ->
%%     ParamsStr = lists:flatten(io_lib:format("~p.", [Params])),
%%     billy_transaction_db:update(#{transaction_id => TrId, params => ParamsStr});



%%%===================================================================
%%% Mnesia API
%%%===================================================================

check_cache(#{transaction_id := TrId}) ->
    %% Получить транзакцию из кэша
    CacheTrRes = get_cache(#{transaction_id => TrId}),
    case CacheTrRes of
	%% Если записи нет, получаем транзакцию из базы и помещаем в кэш
	[] -> case billy_transaction_db:get(#{transaction_id => TrId}) of
		  {ok, [Tr | _]} -> insert_cache(as_record(Tr));
		  {error, not_found} -> {error, not_found}
	      end;
	%% Если запись есть, ничего не делаем
	[CacheTrRec | _] when is_record(CacheTrRec, billy_transaction) -> ok
    end.


insert_cache(TrRec) when is_record(TrRec, billy_transaction) ->
    F = fun() -> mnesia:write(transaction_cache, TrRec, write) end,
    mnesia:activity(transaction, F).


update_cache(#{transaction_id := TrId, old_ipn_process_stage := OldIpnStage, new_ipn_process_stage := NewIpnStage}) ->
    Pattern = #billy_transaction{_ = '_', transaction_id = TrId},
    F = fun() -> 
 		case mnesia:match_object(transaction_cache, Pattern, write) of
		    %% Транзакция не найдена
		    [] -> {error, not_found};

		    %% Транзакция найдена и старый статус соответствует
 		    [Tr] when is_record(Tr, billy_transaction),Tr#billy_transaction.ipn_process_stage==OldIpnStage ->
			NewTr = Tr#billy_transaction{ipn_process_stage=NewIpnStage},
			ok = mnesia:write(transaction_cache, NewTr, write),
			{ok, NewIpnStage};

		    %% Транзакция найдена, но старый статус не соответствует
		    [Tr] when is_record(Tr, billy_transaction) ->
			{error, {bad_old_stage, Tr#billy_transaction.ipn_process_stage}}
 		end
	end,
    mnesia:activity(transaction, F);


update_cache(#{transaction_id := TrId, old_status := OldStatus, new_status := NewStatus}) ->
    Pattern = #billy_transaction{_ = '_', transaction_id = TrId},
    F = fun() -> 
 		case mnesia:match_object(transaction_cache, Pattern, write) of
		    %% Транзакция не найдена
		    [] -> {error, not_found};

		    %% Транзакция найдена и старый статус соответствует
 		    [Tr] when is_record(Tr, billy_transaction),Tr#billy_transaction.status==OldStatus ->
			NewTr = Tr#billy_transaction{status=NewStatus},
			ok = mnesia:write(transaction_cache, NewTr, write),
			{ok, NewStatus};

		    %% Транзакция найдена, но старый статус не соответствует
		    [Tr] when is_record(Tr, billy_transaction) ->
			{error, {bad_old_status, Tr#billy_transaction.status}}
 		end
	end,
    mnesia:activity(transaction, F);


update_cache(#{transaction_id := TrId, ipn_process_date := IpnProcessDate}) ->
    F = fun() -> 
		MatchPattern = #billy_transaction{_ = '_', transaction_id = TrId},
 		case mnesia:match_object(transaction_cache, MatchPattern, write) of
		    [] -> {error, not_found};
 		    [Tr] when is_record(Tr, billy_transaction) ->
			NewTr = Tr#billy_transaction{ipn_process_date=IpnProcessDate},
			mnesia:write(transaction_cache, NewTr, write)
 		end
	end,
    mnesia:activity(transaction, F);

update_cache(#{transaction_id := TrId, ipn_process_stage := IpnProcessStage}) ->
    F = fun() -> 
		MatchPattern = #billy_transaction{_ = '_', transaction_id = TrId},
 		case mnesia:match_object(transaction_cache, MatchPattern, write) of
		    [] -> {error, not_found};
 		    [Tr] when is_record(Tr, billy_transaction) ->
			NewTr = Tr#billy_transaction{ipn_process_stage=IpnProcessStage},
			mnesia:write(transaction_cache, NewTr, write)
 		end
	end,
    mnesia:activity(transaction, F).


get_cache(#{transaction_id := TrId}) ->
    F = fun() -> mnesia:read({transaction_cache, TrId})  end,
    mnesia:activity(transaction, F).





%%%===================================================================
%%% Internal functions
%%%===================================================================


as_proplist(Tr) when is_record(Tr, billy_transaction) ->
    billy_conv:record_as_proplist(Tr, record_info(fields, billy_transaction)).

as_record(Tr) when is_record(Tr, billy_transaction) -> Tr;
as_record(Tr) when is_list(Tr) ->
    billy_conv:proplist_as_record(Tr, billy_transaction, record_info(fields, billy_transaction)).



