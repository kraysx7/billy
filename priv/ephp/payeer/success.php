<?php
  $transaction = get_transaction((int)$_GET['m_orderid']);
  $merchant = get_merchant((int)$transaction['user_id']);

  echo '<meta http-equiv="refresh" content="0;url='.$merchant['params']['success_url'].'">';
  
  /*if (isset($_GET['m_operation_id']) && isset($_GET['m_sign']))
  {
    $transaction = get_transaction((int)$_GET['m_orderid']);
    $merchant = get_merchant((int)$transaction['user_id']);

    $m_key = get_config("payeer_secret_key");
    $arHash = array(
      $_GET['m_operation_id'],
      $_GET['m_operation_ps'],
      $_GET['m_operation_date'],
      $_GET['m_operation_pay_date'],
      $_GET['m_shop'],
      $_GET['m_orderid'],
      $_GET['m_amount'],
      $_GET['m_curr'],
      $_GET['m_desc'],
      $_GET['m_status']
    );

    if (isset($_GET['m_params']))
    {
      $arHash[] = $_GET['m_params'];
    }
    $arHash[] = $m_key;

    $sign_hash = strtoupper(hash('sha256', implode(':', $arHash)));

    if ($_GET['m_sign'] == $sign_hash &&
        $_GET['m_status'] == 'success' &&
        $transaction['params']['payee_cost'] == (int)($_GET['m_amount']*100) &&
        $transaction['params']['payee_ccy'] == $_GET['m_curr'])
    {
      // $result = process_transaction((int)$_GET['m_orderid'], 'success');
      echo '<meta http-equiv="refresh" content="0;url='.$merchant['params']['success_url'].'">';      
    }
    exit($_GET['m_orderid'].'|error');
  }*/?>