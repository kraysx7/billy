<?php

  // Получаем транзакцию
  $transaction_id = (int)$_GET["tr_id"];
  $transaction = get_transaction($transaction_id)

  // Получаем мерчанта
  $merchant_id = (int)$transaction["merchant_id"];
  $merchant = get_merchant($merchant_id);

  $tr_status = (int)$transaction["status"];
  if($tr_status == 0) {

    // Получаем настройки мерчанта для данной платёжной системы
    $paysystem_config = get_paysystem_config($merchant_id, $transaction["params"]["system"]);

    $connectum_user = $paysystem_config["api_user"];
    $connectum_password = $paysystem_config["api_password"];

    $headers = array(
      "Content-Type" => "application/json",
      "Authorization" => "Basic ".base64_encode($connectum_user.":".$connectum_password)
    );

    $connectum_certs_path = $paysystem_config["certs_path"];
    $options = array(
      "ssl_options" => array(
        "cacertfile" => "priv/trusted_certs/".$connectum_certs_path."/cacertfile.cer",
        "certfile" => "priv/trusted_certs/".$connectum_certs_path."/certfile.crt",
        "keyfile" => "priv/trusted_certs/".$connectum_certs_path."/keyfile.key"
      )
    );

    $connectum_order_id =  (int)$transaction["params"]["connectum_order_id"];

    $connectum_api_url = $paysystem_config["api_url"];
    $get_order_query = $connectum_api_url."/orders/".$connectum_order_id."?expand=card,client,location,custom_fields,issuer,secure3d,operations.cashflow";

    $get_order_res = curl("get", $get_order_query, $headers, "", $options);

    $code = $get_order_res["code"];
    if($code == 200 || $code == 201) {

      $get_order_response0 = $get_order_res["response"];
      $get_order_response = json_decode($get_order_response0);

      $get_order_amount = (int)((float)($get_order_response["orders"][0]["amount"]) * 100);
      $get_order_ccy = $get_order_response["orders"][0]["currency"];

      if($transaction['params']['payee_cost'] == $get_order_amount  &&
         $transaction['params']['payee_ccy'] == $get_order_ccy)
      {
          $get_order_status = $get_order_response["orders"][0]["status"];
          if($get_order_status == "authorized") {
            $curl_charge_query = $connectum_api_url."/orders/".$connectum_order_id."/charge";
            $charge_res = curl("put", $curl_charge_query, $headers, "", $options);
            $charge_code = $charge_res["code"];
            if($charge_code == 200 || $charge_code == 201) {
              $charge_response0 = $charge_res["response"];
              $charge_response = json_decode($charge_response0);
              $charge_order_status = $charge_response["orders"][0]["status"];
              if($charge_order_status == "charged") {
                process_transaction($transaction_id, 'success');
                header("location", $merchant['params']['success_url']);
              } else {
                process_transaction($transaction_id, 'fail');
                header("location", $merchant['params']['fail_url']);
              }
            }
          }
          if($get_order_status == "charged") {
            process_transaction($transaction_id, 'success');
            header("location", $merchant['params']['success_url']);
          }
          else if($get_order_status == "rejected") {
            process_transaction($transaction_id, 'fail');
            header("location", $merchant['params']['fail_url']);
          }
          else if($get_order_status == "declined") {
            process_transaction($transaction_id, 'fail');
            header("location", $merchant['params']['fail_url']);
          }
          else if($get_order_status == "error") {
            process_transaction($transaction_id, 'fail');
            header("location", $merchant['params']['fail_url']);
          }
          else if($get_order_status == "fraud") {
            process_transaction($transaction_id, 'fail');
            header("location", $merchant['params']['fail_url']);
          }
          else {
            header("location", $merchant['params']['fail_url']);
          }
      }
      else {
        process_transaction($transaction_id, 'fail');
        header("location", $merchant['params']['fail_url']);
      }
    }
  }
  else {
    header("location", $merchant['params']['success_url']);
  }
?>
