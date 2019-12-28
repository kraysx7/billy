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

  // Получаем данные о мерчанте
  $merchant = get_merchant($merchant_id);

  // Получаем настройки мерчанта для данной платёжной системы
  $paysystem_config = get_paysystem_config($merchant_id, $transaction["params"]["system"]);

  $m_shop = $paysystem_config["payeer_merchant_id"];
  $m_key = $paysystem_config["payeer_secret_key"];

  $m_orderid = $tr_id;
  $m_amount = number_format((float)($transaction['params']['payee_cost']/100), 2);
  $m_curr = $transaction['params']['payee_ccy'];
  $m_desc = base64_encode('Payments for '.$merchant['name']);

  $arHash = array(
    $m_shop,
    $m_orderid,
    $m_amount,
    $m_curr,
    $m_desc,
    $m_key
  );

  $sign = strtoupper(hash('sha256', implode(':', $arHash)));
?>
<meta charset="utf-8">
<script src="https://code.jquery.com/jquery-1.12.4.min.js" integrity="sha256-ZosEbRLbNQzLpnKIkEdrPv7lOy9C27hHQ+Xp8a4MxAQ=" crossorigin="anonymous"></script>
<script>  $(function () {    $.redirectPost("https://payeer.com/merchant/", {      "m_shop":"<?=$m_shop;?>",      "m_orderid":"<?=$m_orderid?>",      "m_amount":"<?=$m_amount?>",      "m_curr":"<?=$m_curr?>",      "m_desc":"<?=$m_desc?>",      "m_sign":"<?=$sign?>"    });  });  $.extend(  {    redirectPost: function(location, args)    {      var form = '';      $.each( args, function( key, value ) {        form += '<input type="hidden" name="'+key+'" value="'+value+'">';      });      $('<form action="' + location + '" method="POST">' + form + '</form>').appendTo($(document.body)).submit();    }  });</script>
