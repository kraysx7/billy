-module(billy_mp_qiwi_handler).

-export([get_balance/1, check_opportunity/1, process_payment/1]).

-include("../include/billy_transaction.hrl").

get_balance(#{ccy_alpha := CcyAlpha, config_group_id := ConfigGroupId}) ->
    MassPaymentsConfig = billy_config:get(masspayments_config),
    
    QiwiConfig = maps:get(qiwi, MassPaymentsConfig),
    
    [Config | _] = lists:filter(fun(ConfigGroup) -> 
					maps:get(group_id, ConfigGroup, 0) == ConfigGroupId
				end, QiwiConfig),

    %% Получить сумму денег на qiwi кошельке
    case get_qiwi_balance(#{config => Config, ccy_alpha => CcyAlpha}) of

	{ok, Balance} -> 

	    QiwiApiPersonId = list_to_binary(maps:get(qiwi_api_personid, Config)),
	    FBalance = list_to_binary(float_to_list(Balance / 100, [{decimals, 2}])),

	    BalanceInfoMap = #{balance => Balance, system_wallet => QiwiApiPersonId},

	    {ok, BalanceInfoMap};

	%% Не хватает денег на кошельке
	{error, low_balance} -> {error, low_balance};

	%% Ошибка вызова API...
	{error, unknown} -> {error, unknown}
    end.



check_opportunity(#{amount := Amount, ccy_alpha := CcyAlpha, address := Address, config_group_id := ConfigGroupId}) ->

    MassPaymentsConfig = billy_config:get(masspayments_config),
    
    QiwiConfig = maps:get(qiwi, MassPaymentsConfig),
    
    [Config | _] = lists:filter(fun(ConfigGroup) -> 
					maps:get(group_id, ConfigGroup, 0) == ConfigGroupId
				end, QiwiConfig),

    FAmount = list_to_binary(float_to_list(Amount / 100, [{decimals, 2}])),
    lager:info("CHECK OPPORTUNITY QIWI need_amount = ~ts ~ts~n", [FAmount, CcyAlpha]),

    %% проверить сумму денег на qiwi кошельке
    case check_qiwi_account(#{config => Config, need_amount => Amount, ccy_alpha => CcyAlpha}) of

	ok -> ok;

	%% Не хватает денег на кошельке
	{error, low_balance} -> {error, low_balance};

	%% Ошибка вызова API...
	{error, unknown} -> {error, unknown}
    end.





process_payment(#{method := steam} = P) ->
    process_payment_(maps:merge(P, #{provider_id => 25549}));

process_payment(#{method := qiwi, address := Address0} = P) ->
    Address = list_to_binary(io_lib:format("+~ts", [Address0])),
    process_payment_(maps:merge(P, #{provider_id => 99, address => Address})).



process_payment_(#{method := Method, provider_id := ProviderId, merchant_id := MerchantId, mp_order_id := MpOrderId, bill_id := BillId, config_group_id := ConfigGroupId, amount := Amount, ccy_alpha := CcyAlpha, ccy_number := CcyNumber, address := Address}) ->
    
    MassPaymentsConfig = billy_config:get(masspayments_config),
    
    QiwiConfig = maps:get(qiwi, MassPaymentsConfig),
    
    [Config | _] = lists:filter(fun(ConfigGroup) -> 
					maps:get(group_id, ConfigGroup, 0) == ConfigGroupId
				end, QiwiConfig),
    
    %% Получить мерчанта, связаного с транзакцией
    {ok, [MerchantUserProplist]} = billy_merchant:get(#{merchant_id => MerchantId}),
    MerchName = proplists:get_value(<<"name">>, MerchantUserProplist),

    %% проверить сумму денег на qiwi кошельке
    case check_qiwi_account(#{config => Config, need_amount => Amount, ccy_alpha => CcyAlpha}) of
	ok ->
	    QiwiOauthToken = maps:get(qiwi_oauth_token, Config, ""),
	    
	    %% Создать транзакцию перевода денег
	    CreateTrParams = #{merchant_id => MerchantId, mp_order_id => MpOrderId, bill_id => BillId, cost => Amount, ccy_alpha => CcyAlpha, ccy_number => CcyNumber},
	    {ok, TrId} = billy_mp_commons:create_masspayment_transaction(CreateTrParams),
	    
	    %% Формируем авторизационный заголовок
	    AuthorizationHeaderData = io_lib:format("Bearer ~ts", [QiwiOauthToken]),
	    FTrCostSumm = list_to_binary(float_to_list(Amount / 100, [{decimals,2}])),
	    FTrId = list_to_binary(lists:flatten(io_lib:format("~p", [TrId]))),
	    
	    DataMap = #{
	      <<"id">> => FTrId, 
	      <<"sum">> => #{<<"amount">> => FTrCostSumm, <<"currency">> => <<"643">>},
	      <<"paymentMethod">> => #{<<"type">> => <<"Account">>, <<"accountId">> => <<"643">>},
	      <<"fields">> => #{<<"account">> => Address}
	     },
	    
	    DataJsonBin = jiffy:encode(DataMap),
	    
	    %% Отправить деньги
	    QueryUrlTpl = "https://edge.qiwi.com/sinap/api/v2/terms/~p/payments",
	    QueryUrl = io_lib:format(QueryUrlTpl, [ProviderId]),
	    
	    ReqHeaders = [
			  {"Authorization", AuthorizationHeaderData}, 
			  {<<"Content-Type">>, <<"application/json">>},
			  {"Accept", "application/json"}
			 ],

	    QueryRes = case hackney:request(post, QueryUrl, ReqHeaders, DataJsonBin, []) of
	    		   {ok, 200, _RespHeaders, ClientRef} ->

			       {ok, RespBody} = hackney:body(ClientRef),
	    		       DecodedRespBody = jiffy:decode(RespBody, [return_maps]),

			       AddressFrom = maps:get(qiwi_api_personid, Config),
	    		       lager:info("[200] FINISH MASSPAYMENT >>> merchant: ~ts, system: qiwi->~p | ~ts ~ts sended from ~ts to ~ts , (tr_id: ~p)~n", [MerchName, Method, FTrCostSumm, CcyAlpha, AddressFrom, Address, TrId]),

			       %% Обновить текущий этап ipn в кэше
			       ok = billy_transaction:update(#{transaction_id => TrId, ipn_process_stage => ?IPN_PROCESS_STAGE_HOT, mode => cache}),
			       
			       LocalTime = calendar:local_time(),
			       
			       %% Обновить статус транзакции и код результата обработки в базе
			       {ok, 1} = billy_transaction:update(#{transaction_id => TrId, process_result_code => ?TR_PROCESS_RESULT_SUCCESS, status => ?TR_STATUS_PROCESSED}),
			       
			       %% Обновить текущий этап ipn и дату начала в бд
			       {ok, 1} = billy_transaction:update(#{transaction_id => TrId, ipn_process_stage => ?IPN_PROCESS_STAGE_HOT, ipn_process_date => LocalTime}),
			       

			       %% Закрыть заказ на вывод (УСПЕШНО)
			       CloseOrderParams = [calendar:local_time(), 1, MpOrderId],
			       billy_mysql:exec_prepared_stmt(#{stmt => close_masspayment_order_stmt, params => CloseOrderParams}),
			       			       
	    		       {ok, TrId};
	    		   {ok, 400, _RespHeaders, ClientRef} ->

			       {ok, RespBody} = hackney:body(ClientRef),
	    		       DecodedRespBody = jiffy:decode(RespBody, [return_maps]),

	    		       ErrorCode = maps:get(<<"code">>, DecodedRespBody, <<"">>),
			       AddressFrom = maps:get(qiwi_api_personid, Config),
	    		       lager:error("[400] ERROR MASSPAYMENT (code=~ts) >>> merchant: ~ts, system: qiwi->~p | ~ts ~ts not sended from ~ts to ~ts , (tr_id: ~p)~n", [ErrorCode, MerchName, Method, FTrCostSumm, CcyAlpha, AddressFrom, Address, TrId]),

			       %% Обновить текущий этап ipn в кэше
			       ok = billy_transaction:update(#{transaction_id => TrId, ipn_process_stage => ?IPN_PROCESS_STAGE_HOT, mode => cache}),
			       
			       LocalTime = calendar:local_time(),
			       
			       %% Обновить статус транзакции и код результата обработки в базе
			       {ok, 1} = billy_transaction:update(#{transaction_id => TrId, process_result_code => ?TR_PROCESS_RESULT_FAIL, status => ?TR_STATUS_PROCESSED}),
			       
			       %% Обновить текущий этап ipn и дату начала в бд
			       {ok, 1} = billy_transaction:update(#{transaction_id => TrId, ipn_process_stage => ?IPN_PROCESS_STAGE_HOT, ipn_process_date => LocalTime}),
			       

			       %% Закрыть заказ (ОШИБКА)
			       CloseOrderParams = [calendar:local_time(), 1, MpOrderId],
			       billy_mysql:exec_prepared_stmt(#{stmt => close_masspayment_order_stmt, params => CloseOrderParams}),

	    		       case ErrorCode of
	    			   <<"QWPRC-1018">> -> {error, address_error};
	    			   <<"QWPRC-934">> -> {error, address_error};
	    			   <<"QWPRC-319">> -> {error, address_error};
	    			   <<"QWPRC-300">> -> {error, address_error};

				   <<"QWPRC-780">> -> {error, address_error}; %% СБ
	    			   _ -> {error, unknown}
	    		       end;
	    		   Resp -> 
	    		       lager:error("ERROR MASSPAYMENT RESPONSE: ~p~n", [Resp]),
	    		       {error, unknown}
	    	       end,
	    QueryRes;

	%% Не хватает денег на кошельке
	{error, low_balance} -> {error, low_balance};

	%% Ошибка вызова API...
	{error, unknown} -> {error, unknown}
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================


get_qiwi_balance(#{config := Config, ccy_alpha := CcyAlpha}) ->
    case get_qiwi_account_info(#{config => Config}) of
	{ok, AccountsInfo} ->
	    Accounts = maps:get(<<"accounts">>, AccountsInfo),
	    [RubAsset | _] = lists:filtermap(fun(E) -> 
						     case maps:get(<<"alias">>, E) of
							 <<"qw_wallet_rub">> -> true;
							 _ -> false
						     end
					     end, Accounts),
	    case RubAsset of
		[] -> {error, not_found};
		RubAsset when is_map(RubAsset)->
		    BalanceMap = maps:get(<<"balance">>, RubAsset),
		    BalanceAmount = maps:get(<<"amount">>, BalanceMap, 0),
		    BalanceAmountCents = BalanceAmount * 100,

		    QiwiApiPersonId = maps:get(qiwi_api_personid, Config),

		    lager:info("GET QIWI BALANCE : ~ts balance = ~p ~ts ~n", [QiwiApiPersonId, BalanceAmount, CcyAlpha]),

		    {ok, BalanceAmountCents}
	    end;
	%% Ошибка вызова API...
	{error, unknown} -> {error, unknown}
    end.
    
    


check_qiwi_account(#{config := Config, need_amount := NeedAmount, ccy_alpha := CcyAlpha}) ->
    case get_qiwi_account_info(#{config => Config}) of
	{ok, AccountsInfo} ->
	    Accounts = maps:get(<<"accounts">>, AccountsInfo),
	    [RubAsset | _] = lists:filtermap(fun(E) -> 
						     case maps:get(<<"alias">>, E) of
							 <<"qw_wallet_rub">> -> true;
							 _ -> false
						     end
					     end, Accounts),
	    case RubAsset of
		[] -> {error, not_found};
		RubAsset when is_map(RubAsset)->
		    BalanceMap = maps:get(<<"balance">>, RubAsset),
		    BalanceAmount = maps:get(<<"amount">>, BalanceMap, 0),
		    BalanceAmountCents = BalanceAmount * 100,

		    QiwiApiPersonId = maps:get(qiwi_api_personid, Config),

		    FNeedAmount = list_to_binary(float_to_list(NeedAmount / 100, [{decimals,2}])),
		    lager:info("CHECK QIWI BALANCE : ~ts balance = ~p ~ts ; need_amount = ~ts ~ts~n", [QiwiApiPersonId, BalanceAmount, CcyAlpha, FNeedAmount, CcyAlpha]),

		    case BalanceAmountCents - NeedAmount of
			D when D < 0 -> {error, low_balance};
			D when D >= 0 -> ok
		    end
	    end;
	%% Ошибка вызова API...
	{error, unknown} -> {error, unknown}
    end.


%% Получаем информацию о состоянии счётов qiwi
get_qiwi_account_info(#{config := Config}) ->

    QiwiApiPersonId = maps:get(qiwi_api_personid, Config),
    QiwiOauthToken = maps:get(qiwi_oauth_token, Config),

    %% Формируем авторизационный заголовок
    AuthorizationHeaderData = io_lib:format("Bearer ~ts", [QiwiOauthToken]),

    %% Получить информацию о счетах
    QueryUrlTpl = "https://edge.qiwi.com/funding-sources/v2/persons/~ts/accounts",
    QueryUrl = io_lib:format(QueryUrlTpl, [QiwiApiPersonId]),

    Headers = [
	       {"Authorization", AuthorizationHeaderData}, 
	       {"Content-Type", "application/json"},
	       {"Accept", "application/json"}
	      ],
    
    %% Вызываем API qiwi и получаем данные по кошельку
    QueryRes = case hackney:request(get, QueryUrl, Headers, <<>>, []) of

		   %% Запрос выполнен успешно
		   {ok, 200, _RespHeaders, ClientRef}=HR ->
		       {ok, RespBody} = hackney:body(ClientRef),
		       DecodedRespBody = jiffy:decode(RespBody, [return_maps]),
		       {ok, DecodedRespBody};

		   Resp -> 
		       lager:error("ERROR GET QIWI ACCOUNT INFO: ~p~n", [Resp]),
		       {error, unknown}
	       end,
    QueryRes.



