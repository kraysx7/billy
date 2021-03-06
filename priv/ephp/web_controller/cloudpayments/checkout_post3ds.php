<?php

  //Получаем транзакцию
  $tr_id = (int)$_POST['MD'];
  $transaction = get_transaction($tr_id);

  if(count($transaction) == 0) {
    echo "Transaction error";
    exit;
  }

  // Получаем настройки мерчанта для данной платёжной системы
  $merchant_id = $transaction["merchant_id"];
  $paysystem_config = get_paysystem_config($merchant_id, $transaction["params"]["system"]);

  $publicID = $paysystem_config["public_id"];
  $secretKey = $paysystem_config["secret_key"];

  $options = array();
  $headers = array(
    "Content-Type" => "application/x-www-form-urlencoded",
    "Authorization" => "Basic ".base64_encode($publicID.":".$secretKey)
  );

  $postData = 'TransactionId='.$_POST['MD'].'&PaRes='.http_uri_encode($_POST['PaRes']);
  $curl_res = curl("post", "https://api.cloudpayments.ru/payments/cards/post3ds", $headers, $postData, $options);

  $code = $curl_res["code"];
  $response = $curl_res["response"];

  $jsonData = json_decode($response);

  if($jsonData['Success'] == true &&
    $transaction['params']['payee_cost'] == (int)($jsonData['Model']['Amount']*100) &&
    $transaction['params']['payee_ccy'] == $jsonData['Model']['Currency'] &&
    $jsonData['Model']['StatusCode'] == 3) {
    $result = process_transaction((int)$jsonData['Model']['InvoiceId'], 'success');
    echo '<meta http-equiv="refresh" content="0;url='.$merchant['params']['success_url'].'">';
  } else {
    $result = process_transaction((int)$jsonData['Model']['InvoiceId'], 'fail');
    echo '<meta http-equiv="refresh" content="0;url='.$merchant['params']['fail_url'].'">';
  }

?>
