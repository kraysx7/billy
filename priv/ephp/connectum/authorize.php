<?php

  $transaction_id = (int)$_POST['tr_id'];

  if(count($_POST) == 0 || check_billy_signature($transaction_id, $_POST['signature']) == false) {
    echo "Gateway signature error!";
    exit;
  }

  $connectum_user = get_config("connectum_api_user");
  $connectum_password = get_config("connectum_api_password");

  $headers = array(
    "Content-Type" => "application/json",
    "Authorization" => "Basic ".base64_encode($connectum_user.":".$connectum_password)
  );

  $connectum_certs_path = get_config("connectum_certs_path");
  $options = array(
    "ssl_options" => array(
      "cacertfile" => "../priv/trusted_certs/".$connectum_certs_path."/cacertfile.cer",
      "certfile" => "../priv/trusted_certs/".$connectum_certs_path."/certfile.crt",
      "keyfile" => "../priv/trusted_certs/".$connectum_certs_path."/keyfile.key"
    )
  );

  $transaction = get_transaction($transaction_id);

  $post_data = array(
    "amount" =>  $transaction["cost"] / 100,
    "currency" => $transaction["currency_alpha"],
    "merchant_order_id" => $transaction_id,
    "card" => array(
      "holder" => $_POST["card_holder"],
      "expiration_month" => (int)$_POST["card_exp_month"],
      "expiration_year" => (int)("20".$_POST["card_exp_year"]),
      "cvv" => $_POST["card_cvc"]
    ),
    "location" => array(
      "ip" => $transaction["params"]["anti_fraud_params"]["user_ip_str"]
    ),
    "client" => array(
      "email" =>  $transaction["params"]["method_params"]["email"]
    ),
    "options" => array(
      // "autocharge" => 1,
      "force3d" => 1,
      "return_url" => "https://somacase.com/payment/connectum/finish?tr_id=".$transaction_id
    ),
    "pan" => $n_str = str_replace(" ","", $_POST["card_pan"])
  );

  $post_data_json = json_encode($post_data);

  $connectum_api_host = get_config("connectum_api_host");
  $curl_query = $connectum_api_host."/orders/authorize";
  $curl_res = curl("post", $curl_query, $headers, $post_data_json, $options);

  $code = $curl_res["code"];
  // echo $curl_res["response"];
  if($code == 200 || $code == 201) {
    $response = $curl_res["response"];
    $decoded_response = json_decode($response);
    $connectum_order_id = $decoded_response["orders"][0]["id"];

    update_transaction_param($transaction_id, "connectum_order_id", $connectum_order_id);

    echo $response;
  }
?>
