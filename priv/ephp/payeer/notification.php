<?php
  if (isset($_POST['m_operation_id']) && isset($_POST['m_sign']))
  {
    $transaction = get_transaction((int)$_POST['m_orderid']);
    $merchant = get_merchant((int)$transaction['user_id']);

    $m_key = get_config("payeer_secret_key");
    $arHash = array(
      $_POST['m_operation_id'],
      $_POST['m_operation_ps'],
      $_POST['m_operation_date'],
      $_POST['m_operation_pay_date'],
      $_POST['m_shop'],
      $_POST['m_orderid'],
      $_POST['m_amount'],
      $_POST['m_curr'],
      $_POST['m_desc'],
      $_POST['m_status']
    );
    // Если были переданы дополнительные параметры, то добавляем их в 10 массив
    if (isset($_POST['m_params']))
    {
      $arHash[] = $_POST['m_params'];
    }
    // Добавляем в массив секретный ключ
    $arHash[] = $m_key;
    // Формируем подпись
    $sign_hash = strtoupper(hash('sha256', implode(':', $arHash)));
    
    // Если подпись, сумма и валюта совпадают и статус платежа “Выполнен” - закрываем транзакцию успешно
    if ($_POST['m_sign'] == $sign_hash && $_POST['m_status'] == 'success' &&
        $transaction['params']['payee_cost'] == (int)$_POST['m_amount']*100 &&
        $transaction['params']['payee_ccy'] == $_POST['m_curr'])
    {
      process_transaction((int)$_POST['m_orderid'], 'success');
      exit($_POST['m_orderid'].'|success');    
    }
    // В противном случае возвращаем ошибку
    else
    {
      process_transaction((int)$_POST['m_orderid'], 'fail');
      exit($_POST['m_orderid'].'|error');
    }
  }?>