
<html>
<head>
  <?php
    header("content-type", "text/html");
  ?>

  <meta charset="utf-8">
  <script src="/billy/static/javascripts/jquery.min.js"></script>
  <link href ="/billy/static/main.css" rel="stylesheet" type="text/css">
</head>
<body>
  <div class="container">
    <div class="card-wrapper"></div>
    <div class="form-container active">
      <form id="payment3ds" autocomplete="off">
        <input type="text" data-cp="code" placeholder="sms code" class="minified" />
        <button type="button" class="bepay" name="confirm">Подтвердить</button>
      </form>
      <div id="errors"></div>
    </div>
  </div>


  <script>
    $(function () {

    });

    $('button[name="confirm"]').click(function() {
        $(this).attr('disabled', 'disabled');
        var code = $('#payment3ds input[data-cp="code"]').val();

        $.post('/payment/onlinekassa/charge3ds', {
            code: code
        }, function(response) {
            try {
              window.location.replace("/payment/onlinekassa/finish");

              // var jsonData = $.parseJSON(response);
            } catch (error) {
              console.log(error);
            }
        });
    });

  </script>

</body>
</html>
