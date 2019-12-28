<?php 
	$tr_id = (int)($_POST["tr_id"]);
	
	$get_tr_res = get_transaction($tr_id);
	
	print_r($get_tr_res);
	
	print_r(">>>".get_config("g2apay_merchant_email")."<<<");
?>
