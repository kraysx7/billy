<?php
  // Получаем данные о транзакции
  $transaction = get_transaction($billy_transaction_id);

  // Получаем данные о мерчанте
  $merchant_id = (int)$transaction["merchant_id"];
  $merchant = get_merchant($merchant_id);

  // Получаем настройки мерчанта для данной платёжной системы
  $paysystem_config = get_paysystem_config($merchant_id, $transaction["params"]["system"]);

  $g2apay_merchant_email = $paysystem_config["g2apay_merchant_email"];
  $g2apay_api_key = $paysystem_config["g2apay_api_key"];
  $g2apay_secret = $paysystem_config["g2apay_secret"];

  $conn_url = $paysystem_config["conn_url"];
  $success_url = $paysystem_config["success_url"]."?tr_id=".$billy_transaction_id;
  $fail_url = $paysystem_config["fail_url"]."?tr_id=".$billy_transaction_id;

  $authorization_str = $g2apay_api_key.$g2apay_merchant_email.$g2apay_secret;
  $authorization_hash = hash("sha256", $authorization_str);
  $authorization_header_data = format_str("~ts; ~ts", array($g2apay_api_key, $authorization_hash));

  $tr_amount_str = float_to_string($transaction["amount"] / 100, 2);

  $query_hash_params = array($billy_transaction_id, $tr_amount_str, $transaction["ccy_alpha"], $g2apay_secret);
  $query_hash_str = format_str("~p~ts~ts~ts", $query_hash_params);
  $query_hash = hash("sha256", $query_hash_str);

  $query_body_items_params = array($tr_amount_str, $tr_amount_str, $billy_transaction_id, $conn_url);
  $query_body_items_format = "[{\"sku\":\"1\",\"name\":\"Balance Refill\",\"amount\":\"~ts\",\"qty\":\"1\",\"price\":\"~ts\",\"id\":\"~p\",\"url\":\"~ts\"}]";
  $query_body_items_str = format_str($query_body_items_format, $query_body_items_params);

  $query_body_params = array($g2apay_api_key, $query_hash, $billy_transaction_id, $tr_amount_str, $transaction["ccy_alpha"], $fail_url, $success_url, $query_body_items_str);
  $query_body_format = "api_hash=~ts&hash=~ts&order_id=~p&amount=~ts&currency=~ts&url_failure=~ts&url_ok=~ts&items=~ts";
  $query_body_str = format_str($query_body_format, $query_body_params);

  $query_headers = array(
    "Authorization" => $authorization_header_data,
    "Content-Type" => "application/x-www-form-urlencoded"
  );
  $query_options = array();

  $query_url = "https://checkout.pay.g2a.com/index/createQuote";

  $curl_res = curl("post", $query_url, $query_headers, $query_body_str, $query_options);
  $code = $curl_res["code"];
  $response = $curl_res["response"];

  if($code == 200) {
    $decoded_response = json_decode($response);
    $resp_status = $decoded_response["status"];
    if($resp_status == "ok") {
      $payment_link = "https://checkout.pay.g2a.com/index/gateway?token=".$decoded_response["token"];
      $result = array("status" => "ok", "payment_link" => $payment_link);
      echo json_encode($result);
    }
  }
  else {
    $error_message = "Call G2A api http:".$code." response: ".$response;
    $result = array("status" => "error", "message" => $error_message);
    echo json_encode($result);
  }
?>
