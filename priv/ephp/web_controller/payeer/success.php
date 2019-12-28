<?php
  $tr_id = (int)$_GET["m_orderid"];

  // Получаем данные о транзакции
  $transaction = get_transaction($tr_id);

  // Получаем ссылку перенаправления успешного платежа
  $merchant_id = (int)$transaction["merchant_id"];
  $merchant_success_url = get_merchant_config($merchant_id, "success_url");

  header("location", $merchant_success_url);
?>
