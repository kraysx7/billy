<?php

  if(count($_GET) == 0 || check_billy_signature((int)$_GET['tr_id'],$_GET['signature']) == false) {
    echo "Ошибка подписи";
    exit;
  }

  $tr_id = (int)($_GET["tr_id"]);
  $get_tr_res = get_transaction($tr_id);
  $merchant = get_merchant((int)$get_tr_res['user_id']);

  $m_shop = get_config("payeer_merchant_id");
  $m_key = get_config("payeer_secret_key");

  $m_orderid = $tr_id;
  $m_amount = number_format((float)($get_tr_res['params']['payee_cost']/100), 2);
  $m_curr = $get_tr_res['params']['payee_ccy'];
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
<script src="/billy/static/javascripts/jquery.min.js"></script><script>  $(function () {    $.redirectPost("https://payeer.com/merchant/", {      "m_shop":"<?=$m_shop;?>",      "m_orderid":"<?=$m_orderid?>",      "m_amount":"<?=$m_amount?>",      "m_curr":"<?=$m_curr?>",      "m_desc":"<?=$m_desc?>",      "m_sign":"<?=$sign?>"    });  });  $.extend(  {    redirectPost: function(location, args)    {      var form = '';      $.each( args, function( key, value ) {        form += '<input type="hidden" name="'+key+'" value="'+value+'">';      });      $('<form action="' + location + '" method="POST">' + form + '</form>').appendTo($(document.body)).submit();    }  });</script>
