-module(billy_mp_commons).

-export([create_masspayment_transaction/1]). 

create_masspayment_transaction(#{merchant_id := MerchantId, mp_order_id := MpOrderId, bill_id := BillId, cost := Cost, ccy_alpha := CcyAlpha, ccy_number := CcyNumber}) ->
    
    ParamsMap = #{
      mp_order_id => MpOrderId,
      bill_id => BillId
     },

    ParamsStr = jiffy:encode(ParamsMap),

    TrCreateMap = #{
      merchant_id => MerchantId,
      type => 40,
      amount => Cost,
      ccy_alpha => CcyAlpha,
      ccy_number => CcyNumber,
      params => ParamsStr
     },
    
    case billy_transaction:create(TrCreateMap) of
	{ok, BillyTrId} -> {ok, BillyTrId};
	Res ->
	    lager:error("create masspayment transaction error: ~p", [Res]),
	    {error, unknown}
    end.
