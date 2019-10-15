<?php
  header("content-type", "text/html");

  // Получаем id транзакции шлюза
  $tr_id = (int)($_GET["tr_id"]);

  if(count($_GET) == 0 || check_billy_signature($tr_id, $_GET['signature']) == false) {
    echo "Gateway signature error!";
    exit;
  }

  // Получаем данные о транзакции
  $transaction = get_transaction($tr_id);
  $merchant_id = (int)$transaction["merchant_id"];

  // Получаем настройки мерчанта для данной платёжной системы
  $paysystem_config = get_paysystem_config($merchant_id, $transaction["params"]["system"]);

  $cauri_public_key = $paysystem_config["cauri_public_key"];
  $cauri_private_key = $paysystem_config["cauri_private_key"];

  $wt_data_project = $cauri_public_key;
  $wt_data_order_id = $tr_id;
  $wt_data_description = "123";
  $wt_data_user = $transaction["params"]["anti_fraud_params"]["user_id"];
  $wt_data_display_name = "123";
  $wt_data_email = $transaction["params"]["system_params"]["email"];
  $wt_data_phone = "123";
  $wt_data_locale = "en";
  $wt_data_ip = $transaction["params"]["anti_fraud_params"]["user_ip_str"];
  $wt_data_price = float_to_string($transaction["amount"] / 100, 2);
  $wt_data_currency = $transaction["ccy_alpha"];

  $wt_signature_params_array = array(
      $wt_data_project,
      $wt_data_order_id,
      $wt_data_description,
      $wt_data_user,
      $wt_data_display_name,
      $wt_data_email,
      $wt_data_phone,
      $wt_data_locale,
      $wt_data_ip,
      $wt_data_price,
      $wt_data_currency
  );

  $wt_data_signature_str = implode("|", sort($wt_signature_params_array));
  $wt_data_signature = hash_hmac("sha256", $cauri_private_key, $wt_data_signature_str);
?>

<html>
  <head>
    <meta charset="UTF-8" />
  </head>

  <body>
    <script
        src="https://checkout.cauri.com/widget-v1/widget.js" class="cauri-widget"
        data-project="<?=$wt_data_project;?>"
        data-order-id="<?=$wt_data_order_id;?>"
        data-description="<?=$wt_data_description;?>"
        data-user="<?=$wt_data_user;?>"
        data-display-name="<?=$wt_data_display_name;?>"
        data-email="<?=$wt_data_email;?>"
        data-phone="<?=$wt_data_phone;?>"
        data-locale="<?=$wt_data_locale;?>"
        data-ip="<?=$wt_data_ip;?>"
        data-price="<?=$wt_data_price;?>"
        data-currency="<?=$wt_data_currency;?>"
        data-signature="<?=$wt_data_signature;?>">
    </script>
  </body>
</html>
