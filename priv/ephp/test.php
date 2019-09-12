<?php

 // Задаём метод
 $method = "get"; // "post", "put"

 // Так формируем какие надо заголовки
 $headers = array(
    "x-api-signature" => "bar",
    "any_header" => "foo",
 );
 
 // Так вызываем URLку и получаем результат
 $curl_res = curl("get", "https://yandex.ru", $headers, "");
 $code = $curl_res["code"]; // Код ошибки
 $response = $curl_res["response"]; // Тело ответа
 
 print_r($response);
?>
