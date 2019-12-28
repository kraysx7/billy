<!DOCTYPE html>
<html>
 <head>
  <meta charset="utf-8" />  
  
  <script type="text/javascript" src="http://code.jquery.com/jquery-latest.min.js"></script>

		<script type="text/javascript">
				function reload_routes() {
     $.ajax({
      url: '/ephp/manager/api/reload_routes',
      type: 'POST',
      success: function(data) {
       alert('Запрос выполнен успешно');
      },
      error: function() {
       alert('Произошла ошибка! Попробуйте еще раз');
      }
     })
				}
		</script>
  
  </head> 
 <body>
		<input type="button" onclick="reload_routes()" value="Перезагрузить таблицу роутинга" />
		<br />
  <br />
  
  <!--
  <?php $a = 5 * 23; ?>Result for $a = <?=$a?> <br />
  <?php
      $str = '0K3RgtC+INC30LDQutC+0LTQuNGA0L7QstCw0L3QvdCw0Y8g0YHRgtGA0L7QutCw';
      echo base64_decode($str);
  ?>
    <br />
  Post params : <br />
  <?php print_r($_POST); ?>
  <br />
  <br />
  Get params : <br />
  <?php print_r($_GET); ?>-->
  
  
	</body>
</html>