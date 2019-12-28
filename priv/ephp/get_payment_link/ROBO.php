<?php
  // Получаем данные о транзакции
  $transaction = get_transaction($billy_transaction_id);

  // Получаем данные о мерчанте
  $merchant_id = (int)$transaction["merchant_id"];
  $merchant = get_merchant($merchant_id);

  // Получаем настройки мерчанта для данной платёжной системы
  $paysystem_config = get_paysystem_config($merchant_id, $transaction["params"]["system"]);

  $robokassa_merchant = $paysystem_config["robokassa_merchant"];
  $robokassa_pass1 = $paysystem_config["robokassa_pass1"];

  $robo_amount = float_to_string($transaction["amount"] / 100, 2);

  $robo_sign_str = $robokassa_merchant.":".$robo_amount.":".$billy_transaction_id.":".$robokassa_pass1;
  $robo_sign = hash("md5", $robo_sign_str);

  $comment = http_uri_encode(format_str("Order #~p", array($billy_transaction_id)));

  $robo_base_link = "https://auth.robokassa.ru/Merchant/Index.aspx";

  $payment_link_vars = array($robo_base_link, $robokassa_merchant, $robo_amount, $billy_transaction_id, $comment, $robo_sign);
  $payment_link = format_str("~ts?MerchantLogin=~ts&OutSum=~ts&InvId=~p&Desc=~ts&SignatureValue=~ts", $payment_link_vars);

  $result = array("status" => "ok", "payment_link" => $payment_link);
  echo json_encode($result);
?>
