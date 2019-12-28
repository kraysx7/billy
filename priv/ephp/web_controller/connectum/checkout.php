<html>
<meta charset="utf-8">

<?php
  if(count($_GET) == 0 || check_billy_signature((int)$_GET['tr_id'],$_GET['signature']) == false) {
    echo "Gateway signature error!";
    exit;
  }
?>

<script src="/billy/static/javascripts/jquery.min.js"></script>
<link href ="/billy/static/main.css" rel="stylesheet" type="text/css">

<div class="container">
  <div class="card-wrapper"></div>
  <div class="form-container active">
    <form id="paymentForm" autocomplete="off">
      <input type="text" data-card="card_pan" placeholder="Card number" />
      <input type="text" data-card="card_holder" placeholder="Full name" />
      <input type="text" data-card="card_exp_month" placeholder="MM" class="minified" maxlength="2" />
      <input type="text" data-card="card_exp_year"  placeholder="YY" class="minified" maxlength="2" />
      <input type="text" data-card="card_cvc" placeholder="CVC" class="minified" />
      <input type="hidden" name="tr_id" value="<?=$_GET['tr_id'];?>" />
      <button type="button" class="bepay" name="submit">Оплатить</button>
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
      numberInput: 'input[data-card="card_pan"]',
      nameInput: 'input[data-card="card_holder"]',
      expiryInput: 'input[data-card="card_exp_month"], input[data-card="card_exp_year"]',
      cvcInput: 'input[data-card="card_cvc"]'
    },
    formatting: true,
    debug: false
  });
</script>

<script>

  $('button[name="submit"]').click(function() {
      // $(this).attr('disabled', 'disabled');

      var card_pan = $('#paymentForm input[data-card="card_pan"]').val();
      var card_holder = $('#paymentForm input[data-card="card_holder"]').val();
      var card_exp_month = parseInt($('#paymentForm input[data-card="card_exp_month"]').val());
      var card_exp_year = parseInt($('#paymentForm input[data-card="card_exp_year"]').val());
      var card_cvc = $('#paymentForm input[data-card="card_cvc"]').val();

      $.post('/payment/connectum/authorize', {
        tr_id: <?=$_GET['tr_id'];?>,
        signature: '<?=$_GET['signature'];?>',
        card_pan: card_pan,
        card_holder: card_holder,
        card_exp_month: card_exp_month,
        card_exp_year: card_exp_year,
        card_cvc: card_cvc
      }, function(response0) {
        //console.log(response0);
        var response = $.parseJSON(response0);
        console.log(response);
        try {
          $.redirectPost(response.orders[0].form3d.action, {
            MD: response.orders[0].form3d.MD,
            PaReq: response.orders[0].form3d.PaReq,
            TermUrl: response.orders[0].form3d.TermUrl
          });
        } catch (error) {

        }
      });

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
