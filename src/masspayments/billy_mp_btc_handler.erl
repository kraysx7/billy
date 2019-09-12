-module(billy_mp_btc_handler).

-export([check_opportunity/1, process_payment/1]). 




check_opportunity(#{amount := Amount, ccy_alpha := CcyAlpha, address := Address, config_group_id := ConfigGroupId}) ->

    MassPaymentsConfig = billy_config:get(masspayments_config),
    
    PayeerConfig = maps:get(paykassa, MassPaymentsConfig),
    
    [Config | _] = lists:filter(fun(ConfigGroup) -> 
					maps:get(group_id, ConfigGroup, 0) == ConfigGroupId
				end, PayeerConfig),


    FAmount = list_to_binary(float_to_list(Amount / 100000000, [{decimals, 8}])),
    error_logger:info_msg("CHECK OPPORTUNITY BTC (PAYKASSA) need_amount = ~ts ~ts~n", [FAmount, CcyAlpha]),

    %% проверить сумму денег на payeer кошельке
    case check_btc_balance(#{config => Config, need_amount => Amount, ccy => CcyAlpha}) of

	ok -> ok;

	%% Не хватает денег на кошельке
	{error, low_balance} -> {error, low_balance};

	%% Ошибка вызова API...
	{error, unknown} -> {error, unknown}
    end.



process_payment(#{method := btc, merchant_id := MerchantId, mp_order_id := MpOrderId, bill_id := BillId, config_group_id := ConfigGroupId, amount := Amount, ccy_alpha := CcyAlpha, ccy_number := CcyNumber, address := Address}) ->

    MassPaymentsConfig = billy_config:get(masspayments_config),
    
    PayeerConfig = maps:get(paykassa, MassPaymentsConfig),
    
    [Config | _] = lists:filter(fun(ConfigGroup) -> 
					maps:get(group_id, ConfigGroup, 0) == ConfigGroupId
				end, PayeerConfig),

    error_logger:info_msg("PROCESS BTC (PAYKASSA) mp_order_id=~p~n", [MpOrderId]),

    %% Проверить сумму денег на bitcoin кошельке
    case check_btc_balance(#{config => Config, need_amount => Amount, ccy => CcyAlpha}) of
	ok ->

	    %% Получить мерчанта, связаного с транзакцией
	    {ok, [MerchData | _]} = billy_cbserver:get_user(#{user_id => MerchantId, res_type => json}),
	    MerchName = proplists:get_value(<<"name">>, MerchData),

	    %% Создать транзакцию перевода денег
	    CreateTrParams = #{merchant_id => MerchantId, mp_order_id => MpOrderId, bill_id => BillId, cost => Amount, ccy_alpha => CcyAlpha, ccy_number => CcyNumber},
	    {ok, TransactionId} = billy_mp_commons:create_masspayment_transaction(CreateTrParams),

	    FTrCostSumm = list_to_binary(float_to_list(Amount / 100000000, [{decimals,8}])),

	    %% $system_id = [
	    %%     "payeer" => 1, // поддерживаемые валюты RUB, USD    
	    %%     "advcash" => 4, // поддерживаемые валюты RUB, USD    
	    %%     "perfectmoney" => 2, // поддерживаемые валюты USD    
	    %%     "bitcoin" => 11, // поддерживаемые валюты BTC    
	    %%     "ethereum" => 12, // поддерживаемые валюты ETH    
	    %%     "litecoin" => 14, // поддерживаемые валюты LTC    
	    %%     "dogecoin" => 15, // поддерживаемые валюты DOGE    
	    %%     "dash" => 16, // поддерживаемые валюты DASH    
	    %%     "bitcoincash" => 18, // поддерживаемые валюты BCH    
	    %%     "zcash" => 19, // поддерживаемые валюты ZEC    
	    %%     "monero" => 20, // поддерживаемые валюты XMR    
	    %%     "ethereumclassic" => 21, // поддерживаемые валюты ETC    
	    %%     "ripple" => 22, // поддерживаемые валюты XRP    
	    %%     "neo" => 23, // поддерживаемые валюты NEO    
	    %%     "gas" => 24, // поддерживаемые валюты GAS    
	    %%     "berty" => 7, // поддерживаемые валюты RUB, USD    
	    %%     "bitcoinsv" => 25, // поддерживаемые валюты BSV    
	    %%     "waves" => 26, // поддерживаемые валюты WAVES    
	    %%     "tron" => 27, // поддерживаемые валюты TRX    
	    %%     "stellar" => 28, // поддерживаемые валюты XLM    
	    %% ];

	    %% Вызвать API отправки денег
	    QueryUrl0 = "https://paykassa.pro/api/0.5/index.php",
	    
	    PaykassaApiId = maps:get(paykassa_api_id, Config, ""),
	    PaykassaApiKey = maps:get(paykassa_api_key, Config, ""),
	    PaykassaShopId = maps:get(paykassa_shop_id, Config, ""),
	    PaykassaShopName = maps:get(paykassa_shop_name, Config, ""),

	    QueryUrl = io_lib:format("~ts?api_id=~ts&api_key=~ts&shop=~ts&system=~p&amount=~ts&currency=~ts&comment=~ts&paid_commission=~ts&func=~ts",
				     [QueryUrl0, PaykassaApiId, PaykassaApiKey, PaykassaShopId, 11, FTrCostSumm, CcyAlpha, "testpayment", "shop", "api_payment"]),
	    
	    ReqHeaders = [
			  {<<"Content-Type">>, <<"application/x-www-form-urlencoded">>},
			  {<<"Accept">>, <<"application/json">>}
			 ],

	    %% Вызываем API qiwi и получаем данные по кошельку
	    QueryRes = case hackney:request(post, QueryUrl, ReqHeaders, <<>>, [])  of
			   {ok, 200 , _RespHeaders, ClientRef} ->

			       {ok, RespBody} = hackney:body(ClientRef),
			       DecodedRespBody = jiffy:decode(RespBody, [return_maps]),

			       error_logger:info_msg("[200] FINISH MASSPAYMENT >>> merchant: ~ts, system: paykassa | ~ts ~ts sended from ~ts to ~ts ,  (tr_id: ~p)~n", [MerchName, FTrCostSumm, CcyAlpha, PaykassaShopName, Address, TransactionId]),

			       %% Закрыть транзакцию (УСПЕШНО)
			       billy_cbserver:close_transaction(TransactionId, 0, 1),

			       %% Закрыть заказ (УСПЕШНО)
			       CloseOrderParams = [calendar:local_time(), 1, MpOrderId],
			       billy_mysql:exec_prepared_stmt(#{stmt => close_masspayment_order_stmt, params => CloseOrderParams}),

			       {ok, TransactionId};
			   Resp -> 
			       error_logger:info_msg("DEBUG>>> billy_mp_btc_handler:process_payment HTTP ERROR RESPONSE: ~p~n", [Resp]),

			       {error, unknown}
		       end,
	    QueryRes;

	%% Не хватает денег на кошельке
	{error, low_balance} -> {error, low_balance};

	%% Ошибка вызова API...
	{error, unknown} -> {error, unknown}
    end.





check_btc_balance(#{config := Config, need_amount := NeedAmount, ccy := CcyAlpha}) ->
    case get_paykassa_balance_info(#{config => Config}) of
	{ok, BalanceInfoRes} ->
	    BalanceInfo0 = maps:get(<<"data">>, BalanceInfoRes, #{}),
	    BalanceAmountStr = maps:get(<<"bitcoin_btc">>, BalanceInfo0, <<"0.00">>),

	    BalanceAmount = billy_query_helper:check_float(BalanceAmountStr),
	    BalanceAmountSatoshi = BalanceAmount * 100000000,

	    %% Пишем лог
	    PaykassaShopName = maps:get(paykassa_shop_name, Config, ""),
	    FNeedAmount = list_to_binary(float_to_list(NeedAmount / 100000000, [{decimals, 8}])),
	    error_logger:info_msg("CHECK BTC (PAYKASSA) BALANCE : ~ts balance = ~ts ~ts ; need_amount = ~ts ~ts~n", [PaykassaShopName, BalanceAmountStr, CcyAlpha, FNeedAmount, CcyAlpha]),

	    %% Возвращаем результат
	    case BalanceAmountSatoshi - NeedAmount of
		D when D < 0 -> {error, low_balance};
		D when D >= 0 -> ok
	    end;	
	%% Ошибка вызова API...
	{error, unknown} -> {error, unknown}
    end.


%% Получаем информацию о состоянии счётов payeer
get_paykassa_balance_info(#{config := Config}) ->
    %% Получить информацию о счетах
    QueryUrl0 = "https://paykassa.pro/api/0.5/index.php",

    PaykassaApiId = maps:get(paykassa_api_id, Config, ""),
    PaykassaApiKey = maps:get(paykassa_api_key, Config, ""),
    PaykassaShopId = maps:get(paykassa_shop_id, Config, ""),
    PaykassaApiFunc = "api_get_shop_balance",

    QueryUrl = io_lib:format("~ts?api_id=~ts&api_key=~ts&shop=~ts&func=~ts", [QueryUrl0, PaykassaApiId, PaykassaApiKey, PaykassaShopId, PaykassaApiFunc]),

    ReqHeaders = [
		  {<<"Content-Type">>, <<"application/x-www-form-urlencoded">>},
		  {<<"Accept">>, <<"application/json">>}
		 ],

    %% Вызываем API qiwi и получаем данные по кошельку
    QueryRes = case  hackney:request(post, QueryUrl, ReqHeaders, <<>>, []) of
		   {ok, 200, _RespHeaders, ClientRef} ->

		       {ok, RespBody} = hackney:body(ClientRef),
		       DecodedRespBody = jiffy:decode(RespBody, [return_maps]),
		       {ok, DecodedRespBody};

		   Resp -> 

		       error_logger:info_msg("DEBUG>>> billy_mp_btc_handler:get_paykassa_balance_info hackney Resp: ~p~n", [Resp]),

		       {error, unknown}
	       end,
    QueryRes.
