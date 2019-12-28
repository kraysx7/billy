-module(billy_mp_payeer_handler).

-export([check_opportunity/1, process_payment/1]). 




check_opportunity(#{amount := Amount, ccy_alpha := CcyAlpha, address := Address, config_group_id := ConfigGroupId}) ->

    MassPaymentsConfig = billy_config:get(masspayments_config),
    
    PayeerConfig = maps:get(payeer, MassPaymentsConfig),
    
    [Config | _] = lists:filter(fun(ConfigGroup) -> 
					maps:get(group_id, ConfigGroup, 0) == ConfigGroupId
				end, PayeerConfig),

    FAmount = list_to_binary(float_to_list(Amount / 100, [{decimals, 2}])),
    error_logger:info_msg("CHECK OPPORTUNITY PAYERR need_amount = ~ts ~ts~n", [FAmount, CcyAlpha]),

    %% проверить сумму денег на payeer кошельке
    case check_payeer_balance(#{config => Config, need_amount => Amount, ccy => CcyAlpha}) of
	ok ->
	    %% проверить существует ли пользователь, кому отправляем
	    case check_payeer_user(#{config => Config, user_account => Address}) of

		ok -> ok;

		{error, not_found} -> {error, incorrect_address}
	    end;
	%% Не хватает денег на кошельке
	{error, low_balance} -> {error, low_balance};

	%% Ошибка вызова API...
	{error, unknown} -> {error, unknown}
    end.





process_payment(#{method := payeer, merchant_id := MerchantId, mp_order_id := MpOrderId, bill_id := BillId, config_group_id := ConfigGroupId, amount := Amount, ccy_alpha := CcyAlpha, ccy_number := CcyNumber, address := Address}) ->

    MassPaymentsConfig = billy_config:get(masspayments_config),
    
    PayeerConfig = maps:get(payeer, MassPaymentsConfig),
    
    [Config | _] = lists:filter(fun(ConfigGroup) -> 
					maps:get(group_id, ConfigGroup, 0) == ConfigGroupId
				end, PayeerConfig),

    %% проверить сумму денег на payeer кошельке
    case check_payeer_balance(#{config => Config, need_amount => Amount, ccy => CcyAlpha}) of
	ok ->
	    %% проверить существует ли пользователь, кому отправляем
	    case check_payeer_user(#{config => Config, user_account => Address}) of
		ok ->
		    
		    %% Получить мерчанта, связаного с транзакцией
		    {ok, [MerchData | _]} = billy_cbserver:get_user(#{user_id => MerchantId, res_type => json}),
		    MerchName = proplists:get_value(<<"name">>, MerchData),
		    
		    %% Создать транзакцию перевода денег
		    CreateTrParams = #{merchant_id => MerchantId, mp_order_id => MpOrderId, bill_id => BillId, cost => Amount, ccy_alpha => CcyAlpha, ccy_number => CcyNumber},
		    {ok, TransactionId} = billy_mp_commons:create_masspayment_transaction(CreateTrParams),

		    FTrCostSumm = list_to_binary(float_to_list(Amount / 100, [{decimals,2}])),

		    QueryUrl = "https://payeer.com/ajax/api/api.php?transfer",

		    PayeerAccount = maps:get(payeer_account, Config, ""),
		    PayeerApiUserId = maps:get(payeer_api_user_id, Config, ""),
		    PayeerApiSecretKey = maps:get(payeer_api_secret_key, Config, ""),

		    QueryBody = io_lib:format("account=~ts&apiId=~ts&apiPass=~ts&action=transfer&curIn=~ts&sumOut=~ts&curOut=~ts&to=~ts",
					      [PayeerAccount, PayeerApiUserId, PayeerApiSecretKey, CcyAlpha, FTrCostSumm, CcyAlpha, Address]),

		    ReqHeaders = [
				  {<<"Content-Type">>, <<"application/x-www-form-urlencoded">>},
				  {<<"Accept">>, <<"application/json">>}
				 ],
		    
		    %% Вызываем API qiwi и получаем данные по кошельку
		    QueryRes = case hackney:request(post, QueryUrl, ReqHeaders, QueryBody, [])  of
				   {ok, 200 , _RespHeaders, ClientRef} ->

				       {ok, RespBody} = hackney:body(ClientRef),
				       DecodedRespBody = jiffy:decode(RespBody, [return_maps]),

				       error_logger:info_msg("[200] FINISH MASSPAYMENT >>> merchant: ~ts, system: payeer | ~ts ~ts sended from ~ts to ~ts ,  (tr_id: ~p)~n", [MerchName, FTrCostSumm, CcyAlpha, PayeerAccount, Address, TransactionId]),


				       %% Закрыть транзакцию (УСПЕШНО)
				       billy_cbserver:close_transaction(TransactionId, 0, 1),
				       
				       %% Закрыть заказ (УСПЕШНО)
				       CloseOrderParams = [calendar:local_time(), 1, MpOrderId],
				       billy_mysql:exec_prepared_stmt(#{stmt => close_masspayment_order_stmt, params => CloseOrderParams}),
				       
				       {ok, TransactionId};
				   Resp -> 
				       error_logger:info_msg("DEBUG>>> billy_mp_payeer_handler:process_payment HTTP ERROR RESPONSE: ~p~n", [Resp]),

				       {error, unknown}
			       end,
		    QueryRes;
		{error, not_found} -> {error, address_error}
	    end;
	%% Не хватает денег на кошельке
	{error, low_balance} -> {error, low_balance};

	%% Ошибка вызова API...
	{error, unknown} -> {error, unknown}
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================



check_payeer_balance(#{config := Config, need_amount := NeedAmount, ccy := CcyAlpha}) ->
    case get_payeer_balance_info(#{config => Config}) of
	{ok, BalanceInfoRes} ->
	    BalanceInfo0 = maps:get(<<"balance">>, BalanceInfoRes),
	    BalanceInfo = maps:get(CcyAlpha, BalanceInfo0, #{}),
	    BalanceAmountStr = maps:get(<<"DOSTUPNO_SYST">>, BalanceInfo, <<"0.00">>),
	    BalanceAmount = billy_query_helper:check_float(BalanceAmountStr),
	    BalanceAmountCents = BalanceAmount * 100,

	    %% Пишем лог
	    PayeerAccount = maps:get(payeer_account, Config, ""),
	    FNeedAmount = list_to_binary(float_to_list(NeedAmount / 100, [{decimals,2}])),
	    error_logger:info_msg("CHECK PAYEER BALANCE : ~ts balance = ~p ~ts ; need_amount = ~ts ~ts~n", [PayeerAccount, BalanceAmount, CcyAlpha, FNeedAmount, CcyAlpha]),

	    %% Возвращаем результат
	    case BalanceAmountCents - NeedAmount of
		D when D < 0 -> {error, low_balance};
		D when D >= 0 -> ok
	    end;	
	%% Ошибка вызова API...
	{error, unknown} -> {error, unknown}
    end.


check_payeer_user(#{config := Config, user_account := UserAccount}) ->
    case check_payeer_user_query(#{config => Config, user_account => UserAccount}) of
	{ok, CheckRes} ->
	    CheckErrors = maps:get(<<"errors">>, CheckRes),
	    case length(CheckErrors) of
		0 -> ok;
		_ -> {error, not_found}
	    end;
	%% Ошибка вызова API...
	{error, unknown} -> {error, unknown}
    end.


%% Получаем информацию о состоянии счётов payeer
get_payeer_balance_info(#{config := Config}) ->
    %% Получить информацию о счетах
    QueryUrl = "https://payeer.com/ajax/api/api.php?balance",

    PayeerAccount = maps:get(payeer_account, Config, ""),
    PayeerApiUserId = maps:get(payeer_api_user_id, Config, ""),
    PayeerApiSecretKey = maps:get(payeer_api_secret_key, Config, ""),
    QueryBody = io_lib:format("account=~ts&apiId=~ts&apiPass=~ts&action=balance", [PayeerAccount, PayeerApiUserId, PayeerApiSecretKey]),

    ReqHeaders = [
		  {<<"Content-Type">>, <<"application/x-www-form-urlencoded">>},
		  {<<"Accept">>, <<"application/json">>}
		 ],

    %% Вызываем API qiwi и получаем данные по кошельку
    QueryRes = case  hackney:request(post, QueryUrl, ReqHeaders, QueryBody, []) of
		   {ok, 200, _RespHeaders, ClientRef} ->

		       {ok, RespBody} = hackney:body(ClientRef),
		       DecodedRespBody = jiffy:decode(RespBody, [return_maps]),
		       {ok, DecodedRespBody};

		   Resp -> 

		       error_logger:info_msg("DEBUG>>> billy_mp_payeer_handler:get_payeer_balance_info hackney Resp: ~p~n", [Resp]),

		       {error, unknown}
	       end,
    QueryRes.


%% Получаем информацию о состоянии счётов payeer
check_payeer_user_query(#{config := Config, user_account := UserAccount}) ->
    %% Получить информацию о счетах
    QueryUrl = "https://payeer.com/ajax/api/api.php?checkUser",
    
    PayeerAccount = maps:get(payeer_account, Config, ""),
    PayeerApiUserId = maps:get(payeer_api_user_id, Config, ""),
    PayeerApiSecretKey = maps:get(payeer_api_secret_key, Config, ""),
    QueryBody = io_lib:format("account=~ts&apiId=~ts&apiPass=~ts&action=checkUser&user=~ts",
			      [PayeerAccount, PayeerApiUserId, PayeerApiSecretKey, UserAccount]),

    ReqHeaders = [
		  {<<"Content-Type">>, <<"application/x-www-form-urlencoded">>},
		  {<<"Accept">>, <<"application/json">>}
		 ],
    
    %% Вызываем API qiwi и получаем данные по кошельку
    QueryRes = case hackney:request(post, QueryUrl, ReqHeaders, QueryBody, [])  of
		   {ok, 200, _RespHeaders, ClientRef} ->

		       {ok, RespBody} = hackney:body(ClientRef),
		       DecodedRespBody = jiffy:decode(RespBody, [return_maps]),
		       {ok, DecodedRespBody};

		   Resp -> 

		       error_logger:info_msg("DEBUG>>> billy_mp_payeer_handler:check_payeer_user_query hackney Resp: ~p~n", [Resp]),

		       {error, unknown}
	       end,
    QueryRes.
