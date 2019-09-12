<?php
  $transaction = get_transaction((int)$_GET['m_orderid']);
  $merchant = get_merchant((int)$transaction['user_id']);

  echo '<meta http-equiv="refresh" content="0;url='.$merchant['params']['fail_url'].'">';
?>