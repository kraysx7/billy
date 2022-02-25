<html>
<meta charset="utf-8">
<?php
  header("content-type", "text/html");
?>

<script src="/billy/static/javascripts/jquery.min.js"></script>
<script src="https://widget.cloudpayments.ru/bundles/checkout"></script>
<link href ="/billy/static/main.css" rel="stylesheet" type="text/css">
<div class="container">
  <div class="card-wrapper"></div>
  <div class="form-container active">
    <form id="paymentCheckout" autocomplete="off">
      <input type="text" data-cp="cardNumber" placeholder="Card number" />
      <input type="text" data-cp="name" placeholder="Full name" />
      <input type="text" maxlength="2" data-cp="expDateMonth" placeholder="MM" class="minified" />
      <input type="text" maxlength="2" data-cp="expDateYear" placeholder="YY" class="minified"/>
      <input type="text" data-cp="cvv" placeholder="CVV" class="minified" />
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

  });

  $('button[name="bepay"]').click(function() {
      $(this).attr('disabled', 'disabled');
      var card = $('#paymentCheckout input[data-cp="cardNumber"]').val();
      var name = $('#paymentCheckout input[data-cp="name"]').val();
      var exp_month = $('#paymentCheckout input[data-cp="expDateMonth"]').val();
      var exp_year = $('#paymentCheckout input[data-cp="expDateYear"]').val();
      var cvv = $('#paymentCheckout input[data-cp="cvv"]').val();

      $.post('/payment/onlinekassa/charge', {
          card: card,
          name: name,
          exp_month: exp_month,
          exp_year: exp_year,
          cvv: cvv
      }, function(response) {
          try {
            window.location.replace("/payment/onlinekassa/wait3ds");

            // var jsonData = $.parseJSON(response);
          } catch (error) {
            console.log(error);
          }
      });
  });

  /*$.extend(
  {
    redirectPost: function(location, args)
    {
      var form = '';
      $.each( args, function( key, value ) {
        form += '<input type="hidden" name="'+key+'" value="'+value+'">';
      });
      $('<form action="' + location + '" method="POST">' + form + '</form>').appendTo($(document.body)).submit();
    }
  });*/

</script>
</html>
