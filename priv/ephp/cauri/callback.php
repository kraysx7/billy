<?php
  if (isset($_POST['order_id']) && isset($_POST['signature']))
  {
    $transaction_id = (int)$_POST['order_id'];
    $transaction = get_transaction($transaction_id);
    $merchant = get_merchant((int)$transaction['user_id']);

    $cauri_public_key = "".get_config("cauri_public_key");
    $cauri_private_key = "".get_config("cauri_private_key");

    $arHash = array(
        $_POST['project'],
        $_POST['id'],
        $_POST['order_id'],
        $_POST['user'],
        $_POST['price'],
        $_POST['earned'],
        $_POST['currency'],
        $_POST['type'],
        $_POST['status'],
        $_POST['sandbox'],
        $_POST['amount'],
        $_POST['deposit'],
        $_POST['vat_rate'],
        $_POST['vat_amount']
    );

    $sign_hash_str = implode("|", sort($arHash));
    $sign_hash = hash_hmac("sha256", $cauri_private_key, $sign_hash_str);

    // Если подпись, сумма и валюта совпадают и статус платежа “Выполнен” - закрываем транзакцию успешно
    if ($_POST['signature'] == $sign_hash && $_POST['status'] == 'completed' &&
        $transaction['params']['payee_cost'] == (int)$_POST['price']*100 &&
        $transaction['params']['payee_ccy'] == $_POST['currency'])
    {
      process_transaction($transaction_id, 'success');
      exit($transaction_id.'|success');
    }
    // В противном случае возвращаем ошибку
    else
    {
      process_transaction($transaction_id, 'fail');
      exit($transaction_id.'|error');
    }
  }
?>
