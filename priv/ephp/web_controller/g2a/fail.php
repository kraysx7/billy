<?php
  $tr_id = (int)$_GET["tr_id"];

  // Получаем данные о транзакции
  $transaction = get_transaction($tr_id);

  // Получаем ссылку перенаправления ошибочного платежа
  $merchant_id = (int)$transaction["merchant_id"];
  $merchant_fail_url = get_merchant_config($merchant_id, "fail_url");

  header("location", $merchant_fail_url);
?>
