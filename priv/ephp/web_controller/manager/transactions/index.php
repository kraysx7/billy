<!DOCTYPE html>
<html>
 <head>
  <meta charset="utf-8" />  
  
  <script type="text/javascript" src="http://code.jquery.com/jquery-latest.min.js"></script>

		<script type="text/javascript">
				function get_transaction() {
     var tr_id = $("#input_tr_id").val();
     $.ajax({
      url: '/ephp/manager/api/get_transaction',
      type: 'POST',
      data: {
       'tr_id': tr_id 
      },
      success: function(data) {
       // alert('Запрос выполнен успешно' + data);
       $("#span_get_tr_res").text(data);
      },
      error: function() {
       alert('Произошла ошибка! Попробуйте еще раз');
      }
     })
				}
		</script>
  
  </head> 
 <body>
  
  <img src="/static/img/billy_logo.png" style="width: 100px;"/> <br />
  
  <input type="text" id="input_tr_id" />
  <input type="button" onclick="get_transaction()" value="Найти транзакцию" />
		<br />
  <span id="span_get_tr_res"></span>
	</body>
</html>