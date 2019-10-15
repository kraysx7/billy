<?php

  //Получаем транзакцию
  $tr_id = (int)($_POST["tr_id"]);
  $transaction = get_transaction($tr_id);

  // Получаем настройки мерчанта для данной платёжной системы
  $merchant_id = $transaction["merchant_id"];
  $paysystem_config = get_paysystem_config($merchant_id, $transaction["params"]["system"]);

  $publicID = $paysystem_config["public_id"];
  $secretKey = $paysystem_config["secret_key"];

  if(count($transaction) == 0) {
    echo "Transaction error";
    exit;
  }

  $options = array();

  $headers = array(
    "Content-Type" => "application/x-www-form-urlencoded",
    "X-Request-ID" => "1403-2018-".$tr_id."-44",
    "Authorization" => "Basic ".base64_encode($publicID.":".$secretKey)
  );

  $postData =
    "Amount=".($transaction['params']['payee_cost']/100)."&".
    "Currency=".$transaction['params']['payee_ccy']."&".
    "IpAddress=".$transaction['params']['anti_fraud_params']['user_ip_str']."&".
    "InvoiceId=".$tr_id."&".
    "Description=Payments for somacase.com&".
    "AccountId=".$transaction['params']['anti_fraud_params']['user_id']."&".
    "Name=".$_POST['username']."&".
    // "Email=DJVist@gmail.com&".
    "CardCryptogramPacket=".$_POST['cpacket'];

  $curl_res = curl("post", "https://api.cloudpayments.ru/payments/cards/charge", $headers, $postData, $options);
  $code = $curl_res["code"];
  $response = $curl_res["response"];

  echo($response);
?>
