<?php
  // Получаем данные о транзакции
  $transaction = get_transaction($billy_transaction_id);

  // Получаем данные о мерчанте
  $merchant_id = (int)$transaction["merchant_id"];
  $merchant = get_merchant($merchant_id);

  // Получаем настройки мерчанта для данной платёжной системы
  $paysystem_config = get_paysystem_config($merchant_id, $transaction["params"]["system"]);

  // Формируем авторизационный заголовок
  $auth_header_data = "Bearer ".$paysystem_config["qiwi_api_secret_key"];

  $query_headers = array(
    "Authorization" => $auth_header_data,
    "Content-Type" => "application/json",
    "Accept" => "application/json"
  );
  $query_options = array();

  $comment0 = "Order №".$billy_transaction_id;

  $cur_posix_time = time();
  $exp_posix_time = $cur_posix_time + 172800; // add 2 days
  date_default_timezone_set("Europe/Moscow");
  $exp_date1 = date("Y-n-d", $exp_posix_time);
  $exp_date2 = date("h:m:s.000", $exp_posix_time);
  $exp_date = $exp_date1."T".$exp_date2."Z";

  $query_body_arr = array(
    "amount" => array(
      "currency" => $transaction["ccy_alpha"],
      "value" => float_to_string($transaction["amount"] / 100, 2)
    ),
    "comment" => $comment0,
    "expirationDateTime" => $exp_date,
    "customer" => array(
      "phone" => $transaction["params"]["system_params"]["phone"]
    )
  );

  $query_body_str = json_encode($query_body_arr);

  $query_url = "https://api.qiwi.com/partner/bill/v1/bills/".$billy_transaction_id;

  $curl_res = curl("put", $query_url, $query_headers, $query_body_str, $query_options);
  $code = $curl_res["code"];
  $response = $curl_res["response"];

  if($code == 200) {
    $decoded_response = json_decode($response);
    $payUrl = $decoded_response["payUrl"];
    $success_url0 = $paysystem_config["success_url"];
    $success_url = http_uri_encode($success_url0."?order=".$billy_transaction_id);
    $payment_link = $payUrl."&paySource=".$transaction["params"]["system_params"]["pay_source"]."&successUrl=".$success_url;

    $result = array("status" => "ok", "payment_link" => $payment_link);
    echo json_encode($result);
  }
  else {
    $error_message = "Call qiwi api http:".$code." response: ".$response;
    $result = array("status" => "error", "message" => $error_message);
    echo json_encode($result);
  }
?>
