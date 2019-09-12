<?php

  $publicID = get_config("cloudpayments_public_id");
  $secretKey = get_config("cloudpayments_secret_key");

  $tr_id = (int)($_POST["tr_id"]);
  $get_tr_res = get_transaction($tr_id);
  $merchant = get_merchant((int)$get_tr_res['user_id']);

  if(count($get_tr_res) == 0 || count($merchant) == 0) {
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
    "Amount=".($get_tr_res['params']['payee_cost']/100)."&".
    "Currency=".$get_tr_res['params']['payee_ccy']."&".
    "IpAddress=".$get_tr_res['params']['anti_fraud_params']['user_ip_str']."&".
    "InvoiceId=".(int)($_POST["tr_id"])."&".
    "Description=Payments for somacase.com&".
    "AccountId=".$get_tr_res['params']['anti_fraud_params']['user_id']."&".
    "Name=".$_POST['username']."&".
    // "Email=DJVist@gmail.com&".
    "CardCryptogramPacket=".$_POST['cpacket'];

  $curl_res = curl("post", "https://api.cloudpayments.ru/payments/cards/charge", $headers, $postData, $options);
  $code = $curl_res["code"];
  $response = $curl_res["response"];

  echo($response);
?>
