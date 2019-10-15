<?php

  $transaction_id = (int)$_GET['tr_id'];

  // Проверяем сигнатуру
  if(count($_GET) == 0 || check_billy_signature($transaction_id, $_GET['signature']) == false) {
    echo "Gateway signature error!";
    exit;
  }

  // Получаем транзакцию
  $transaction = get_transaction($transaction_id);

  // Получаем настройки мерчанта для данной платёжной системы
  $merchant_id = $transaction["merchant_id"];
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

  $post_data = array(
    "amount" =>  $transaction["amount"] / 100,
    "currency" => $transaction["ccy_alpha"],
    "merchant_order_id" => $transaction_id,
    "location" => array(
      "ip" => $transaction["params"]["anti_fraud_params"]["user_ip_str"]
    ),
    "client" => array(
      "email" =>  $transaction["params"]["system_params"]["email"]
    ),
    "options" => array(
      // "autocharge" => 1,
      "language" => "en",
      "force3d" => 1,
      "return_url" => $paysystem_config["return_url"]."?tr_id=".$transaction_id
    )
  );

  $post_data_json = json_encode($post_data);

  $connectum_api_url = $paysystem_config["api_url"];
  $curl_query = $connectum_api_url."/orders/create";
  $curl_res = curl("post", $curl_query, $headers, $post_data_json, $options);

  $code = $curl_res["code"];
  if($code == 200 || $code == 201) {
    $response = $curl_res["response"];
    $decoded_response = json_decode($response);
    $connectum_order_id = $decoded_response["orders"][0]["id"];

    update_transaction_param($transaction_id, "connectum_order_id", $connectum_order_id);

    header("location", $curl_res['headers']['Location']);

  }
?>
