-module(billy_payment).
-export([process_transaction/1, calc_billyipn_signature/2]).

-include("../include/billy_transaction.hrl").

process_transaction(#{transaction_id := TrId, process_result := <<"success">>}) ->

    %% Обновить статус транзакции в кэше
    case billy_transaction:update(#{transaction_id => TrId,
				    old_status => ?TR_STATUS_OPENED,
				    new_status => ?TR_STATUS_PROCESSED,
				    mode => cache}) of
	{ok, ?TR_STATUS_PROCESSED} -> 

	    %% Обновить текущий этап ipn в кэше
	    ok = billy_transaction:update(#{transaction_id => TrId, ipn_process_stage => ?IPN_PROCESS_STAGE_HOT, mode => cache}),

	    LocalTime = calendar:local_time(),
	    
	    %% Обновить статус транзакции в базе
	    {ok, 1} = billy_transaction:update(#{transaction_id => TrId, process_result_code => ?TR_PROCESS_RESULT_SUCCESS, status => ?TR_STATUS_PROCESSED}),
	    
	    %% Обновить текущий этап ipn и дату начала в бд
	    {ok, 1} = billy_transaction:update(#{transaction_id => TrId, ipn_process_stage => ?IPN_PROCESS_STAGE_HOT, ipn_process_date => LocalTime}),

	    ok;

	_ -> {error, transaction_already_processed}
    end;


process_transaction(#{transaction_id := TrId, process_result := <<"fail">>}) ->
    %% Обновить статус транзакции в кэше
    case billy_transaction:update(#{transaction_id => TrId,
				    old_status => ?TR_STATUS_OPENED,
				    new_status => ?TR_STATUS_PROCESSED,
				    mode => cache}) of
	{ok, ?TR_STATUS_PROCESSED} -> 
	    
	    %% Обновить текущий этап ipn в кэше
	    ok = billy_transaction:update(#{transaction_id => TrId, ipn_process_stage => ?IPN_PROCESS_STAGE_HOT, mode => cache}),

	    LocalTime = calendar:local_time(),
	    
	    %% Обновить статус транзакции и код результата обработки в базе
	    {ok, 1} = billy_transaction:update(#{transaction_id => TrId, process_result_code => ?TR_PROCESS_RESULT_FAIL, status => ?TR_STATUS_PROCESSED}),
	    
	    %% Обновить текущий этап ipn и дату начала в бд
	    {ok, 1} = billy_transaction:update(#{transaction_id => TrId, ipn_process_stage => ?IPN_PROCESS_STAGE_HOT, ipn_process_date => LocalTime}),

	    ok;

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
