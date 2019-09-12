-module(billy_mp_commons).

-export([create_masspayment_transaction/1]). 

create_masspayment_transaction(#{merchant_id := MerchantId, mp_order_id := MpOrderId, bill_id := BillId, cost := Cost, ccy_alpha := CcyAlpha, ccy_number := CcyNumber}) ->
    
    ParamsMap = #{
      mp_order_id => MpOrderId,
      bill_id => BillId
     },

    ParamsStr = jiffy:encode(ParamsMap),

    case billy_cbserver:create_transaction(#{
					      type => 40,
					      params => ParamsStr,
					      user_id => MerchantId,
					      
					      currency_alpha => CcyAlpha,
					      currency_number => CcyNumber,
					      
					      cost => Cost,
					      new_balance => 0,
					      status => 0,
					      create_date => calendar:local_time(),
					      close_date => null
					    }) of
	{ok, TransactionId} -> {ok, TransactionId};
	Res ->
	    error_logger:info_msg("DEBUG>>> billy_mp_commons:create_masspayment_transaction ERROR Res: ~p~n", [Res]), 
	    {error, unknown}
    end.
