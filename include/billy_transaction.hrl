

-define(IPN_PROCESS_STAGE_HOT, 1).
-define(IPN_PROCESS_STAGE_COLD, 2).
-define(IPN_PROCESS_STAGE_INWORK, 3).
-define(IPN_PROCESS_STAGE_COMPLETE, 4).


-define(TR_PROCESS_RESULT_UNKNOWN, 0).
-define(TR_PROCESS_RESULT_SUCCESS, 1).
-define(TR_PROCESS_RESULT_FAIL, 2).


-define(TR_STATUS_OPENED, 0).
-define(TR_STATUS_PROCESSED, 1).
-define(TR_STATUS_CLOSED, 2).


-record(billy_transaction, {
	  transaction_id, 
	  merchant_id, 
	  type,
	  amount, 
	  ccy_alpha, 
	  ccy_number, 
	  params, 
	  create_date, 
	  close_date,
	  process_result_code,
	  ipn_process_date,
	  ipn_process_stage,
	  status
	 }).


