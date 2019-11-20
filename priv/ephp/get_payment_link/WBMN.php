<?php
  // Получаем данные о транзакции
  $transaction = get_transaction($billy_transaction_id);

  // Получаем данные о мерчанте
  $merchant_id = (int)$transaction["merchant_id"];
  $merchant_secret_key = get_merchant_config($merchant_id, "secret_key");

  // Получаем настройки мерчанта для данной платёжной системы
  $paysystem_config = get_paysystem_config($merchant_id, $transaction["params"]["system"]);

  // Считаем сигнатуру транзакции
  $signature_data = "".$transaction["params"]["bill_id"].$transaction["amount"].$transaction["ccy_alpha"];
  $signature = hash_hmac("sha256", $merchant_secret_key, $signature_data);

  // Берём URL, на который подключена данная система и перенаправляем на checkout форму
  $payment_link = $paysystem_config["conn_url"]."/payment/webmoney/checkout?tr_id=".$billy_transaction_id."&signature=".$signature;

  $result = array("status" => "ok", "payment_link" => $payment_link);
  echo json_encode($result);
?>
