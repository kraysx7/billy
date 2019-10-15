<html>
<meta charset="utf-8">
<?php

  header("content-type", "text/html");

  /* TEST CARD NUMBER: 4925000000000087 */

  $tr_id = (int)$_GET['tr_id'];

  if(count($_GET) == 0 || check_billy_signature($tr_id, $_GET['signature']) == false) {
    echo "Ошибка подписи";
    exit;
  }

  //Получаем транзакцию
  $transaction = get_transaction($tr_id);

  // Получаем настройки мерчанта для данной платёжной системы
  $merchant_id = $transaction["merchant_id"];
  $paysystem_config = get_paysystem_config($merchant_id, $transaction["params"]["system"]);

?>

<script src="/billy/static/javascripts/jquery.min.js"></script>
<script src="https://widget.cloudpayments.ru/bundles/checkout"></script>
<link href ="/billy/static/main.css" rel="stylesheet" type="text/css">
<div class="container">
  <div class="card-wrapper"></div>
  <div class="form-container active">
    <form id="paymentForm" autocomplete="off">
      <input type="text" data-cp="cardNumber" placeholder="Card number" />
      <input type="text" data-cp="name" placeholder="Full name" />
      <input type="text" maxlength="2" data-cp="expDateMonth" placeholder="MM" class="minified" />
      <input type="text" maxlength="2" data-cp="expDateYear" placeholder="YY" class="minified"/>
      <input type="text" data-cp="cvv" placeholder="CVC" class="minified" />
      <input type="hidden" name="tr_id" value="<?=$_GET['tr_id'];?>" />
      <button type="button" class="bepay" name="bepay">Оплатить</button>
    </form>
    <div id="errors"></div>
  </div>
</div>

<script src="/billy/static/card.js"></script>
<script>
  var card = new Card({
    form: 'form',
    container: '.card-wrapper',
    formSelectors: {
      numberInput: 'input[data-cp="cardNumber"]',
      expiryInput: 'input[data-cp="expDateMonth"], input[data-cp="expDateYear"]',
      cvcInput: 'input[data-cp="cvv"]',
      nameInput: 'input[data-cp="name"]'
    },
    formatting: true,
    debug: false
  });
</script>

<script>
  $(function () {
    checkout = new cp.Checkout(
    "pk_9f9e69cbf45f19e45a51136b2c6dd",
    document.getElementById("paymentForm"));
  });

  $('button[name="bepay"]').click(function() {
    $(this).attr('disabled', 'disabled');
      var user = $('#paymentForm input[data-cp="name"]').val();
      var result = checkout.createCryptogramPacket();
      if (result.success) {
        $.post('/payment/cloudpayments/cardCharge', {
          signature: '<?=$_GET['signature'];?>',
          username: user,
          tr_id: <?=$_GET['tr_id'];?>,
          cpacket: result.packet
        }, function(response) {
          try {
            var jsonData = $.parseJSON(response);
            if(jsonData.Model.AcsUrl)
            {
              $.redirectPost(jsonData.Model.AcsUrl, {
                MD: jsonData.Model.TransactionId,
                PaReq: jsonData.Model.PaReq,
                TermUrl: <?=$paysystem_config["term_url"]?>
              });
            } else if(jsonData.Model.StatusCode == 3) {
              $.redirectPost(<?=$paysystem_config["card_charge_url"]?>, {"PaRes":"AQ=="});
            } else if(jsonData.Model.StatusCode == 5) {
              $.redirectPost(<?=$paysystem_config["card_charge_url"]?>, {"PaRes":"decline"});
            }
          } catch (error) {
            console.log(error);
          }
        });
      } else {
        $('button[name="bepay"]').removeAttr('disabled');
        $("#errors").html('');
        for (var msgName in result.messages) {
          $("#errors").append("<span>" + result.messages[msgName] + "</span>");
        }
      }
  });

  $.extend(
  {
    redirectPost: function(location, args)
    {
      var form = '';
      $.each( args, function( key, value ) {
        form += '<input type="hidden" name="'+key+'" value="'+value+'">';
      });
      $('<form action="' + location + '" method="POST">' + form + '</form>').appendTo($(document.body)).submit();
    }
  });
</script>
</html>
