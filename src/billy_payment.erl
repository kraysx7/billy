-module(billy_payment).
-export([process_transaction/1, calc_billyipn_signature/2]).

-define(TR_TYPE_ADDBALANCE, 10).


process_transaction(#{transaction_id := TrId, process_result := <<"fail">>}) ->
    %% Получить транзакцию из кэша и обновить её статус
    CheckTrRes = case billy_cbserver:get_transaction(#{transaction_id=>TrId, mode=>cache, res_type=>json}) of
		     {ok, CacheTrProplist0} ->
			 case billy_cbserver:update_transaction(#{transaction_id=>TrId, old_status=>0, new_status=>3, mode=>cache}) of
			     {ok, _NewCacheTr} -> {ok, CacheTrProplist0};
			     _ -> {error, bad_old_status}
			 end;
		     R ->
			 io:format("DEBUG>>> billy_payment:process_transaction  R: ~p~n", [ R]),
			 {error, trasaction_not_found}
		 end,

    case CheckTrRes of
	{ok, CacheTrProplist} -> 
	    TrUserId = proplists:get_value(<<"user_id">>, CacheTrProplist),
	    TrCost = proplists:get_value(<<"cost">>, CacheTrProplist),
	    case billy_cbserver:update_user_balance(TrUserId, TrCost) of
		{ok, NewBalance} ->

		    TrParamsBin = proplists:get_value(<<"params">>, CacheTrProplist),
		    TrParams = jiffy:decode(TrParamsBin, [return_maps]),
		    NewTrParams = maps:merge(TrParams, #{process_result => <<"fail">>}),
		    NewTrParamsBin = jiffy:encode(NewTrParams),

		    error_logger:info_msg("DEBUG>>> billy_payment:process_transaction #~p  NewTrParamsBin: ~ts~n", [TrId, NewTrParamsBin]),

		    billy_cbserver:update_transaction(#{transaction_id => TrId, params => NewTrParamsBin, params_type => json_str}),

		    billy_cbserver:close_transaction(TrId, NewBalance, 3),
		    
		    {ok, NewBalance};
		{error, low_balance, UserBalance} -> 
		    %% billy_cbserver:close_transaction(TrId, UserBalance, ?TR_STATUS_ERROR_LOW_BALANCE),
		    {error, low_balance};
		{error, user_not_found} -> {error, user_not_found}
	    end;
	_ -> {error, transaction_already_processed}
    end;

process_transaction(#{transaction_id := TrId, process_result := <<"success">>}) ->
    %% Получить транзакцию из кэша и обновить её статус
    CheckTrRes = case billy_cbserver:get_transaction(#{transaction_id=>TrId, mode=>cache, res_type=>json}) of
		     {ok, CacheTrProplist0} ->
			 case billy_cbserver:update_transaction(#{transaction_id=>TrId, old_status=>0, new_status=>1, mode=>cache}) of
			     {ok, _NewCacheTr} -> {ok, CacheTrProplist0};
			     _ -> {error, bad_old_status}
			 end;
		     R ->
			 io:format("DEBUG>>> billy_qiwi_handler:process_transaction  R: ~p~n", [ R]),
			 {error, trasaction_not_found}
		 end,

    case CheckTrRes of
	{ok, CacheTrProplist} -> 
	    TrUserId = proplists:get_value(<<"user_id">>, CacheTrProplist),
	    TrCost = proplists:get_value(<<"cost">>, CacheTrProplist),
	    case billy_cbserver:update_user_balance(TrUserId, TrCost) of
		{ok, NewBalance} ->

		    TrParamsBin = proplists:get_value(<<"params">>, CacheTrProplist),
		    TrParams = jiffy:decode(TrParamsBin, [return_maps]),
		    NewTrParams = maps:merge(TrParams, #{process_result => <<"success">>}),
		    NewTrParamsBin = jiffy:encode(NewTrParams),

		    error_logger:info_msg("DEBUG>>> billy_payment:process_transaction #~p  NewTrParamsBin: ~ts~n", [TrId, NewTrParamsBin]),

		    billy_cbserver:update_transaction(#{transaction_id => TrId, params => NewTrParamsBin, params_type => json_str}),

		    billy_cbserver:close_transaction(TrId, NewBalance, 1),
		    
		    {ok, NewBalance};
		{error, low_balance, UserBalance} -> 
		    %% billy_cbserver:close_transaction(TrId, UserBalance, ?TR_STATUS_ERROR_LOW_BALANCE),
		    {error, low_balance};
		{error, user_not_found} -> {error, user_not_found}
	    end;
	_ -> {error, transaction_already_processed}
    end.



    




calc_billyipn_signature(BillySignParams, MerchantSecretKey) ->
    SortedParams = lists:sort(fun(A, B) -> 
				      {Key1, _Val1} = A,
				      {Key2, _Val2} = B,
				      Key1 < Key2
			      end, BillySignParams),
    %% io:format("DEBUG>>> billy_payment:calc_billy_signature sorted params: ~p~n", [SortedParams]),
    SignatureStrHead0 = lists:foldl(fun(P, Str) -> 
					    case P of
						{_, "undefined"} -> Str;
						{salt, _} -> Str;
						{_Key, Value} ->
						    ValueChk = case Value of
								   Value when is_integer(Value) ->
								       unicode:characters_to_binary(
									 lists:flatten(io_lib:format("~p", [Value])), utf8
									);
								   Value when is_float(Value) ->
								       unicode:characters_to_binary(
									 lists:flatten(io_lib:format("~p", [Value])), utf8
									);
								   Value when is_list(Value) ->
								       unicode:characters_to_binary(Value, utf8);
								   Value when is_binary(Value) ->
								       Value
							       end,
						    PipeSymbol = <<"|">>,
						    <<Str/binary, ValueChk/binary, PipeSymbol/binary>>
					    end
				    end, <<"">>, SortedParams),
    
    %% Убрать последний символ '|' в конце строки параметров
    SA = size(SignatureStrHead0)-1,
    <<SignatureStr:SA/binary, _>> = SignatureStrHead0,
    
    ShaKey = unicode:characters_to_binary(MerchantSecretKey, utf8),
    ShaData = SignatureStr,
    SignatureValue = base64:encode(crypto:hmac(sha, ShaKey, ShaData)),
    %% io:format("DEBUG>>> billy_controller:calc_billy_signature value: ~ts~n", [SignatureValue]),
    SignatureValue.  
