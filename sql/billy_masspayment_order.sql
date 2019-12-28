DROP TABLE IF EXISTS `billy_masspayment_order`;
CREATE TABLE `billy_masspayment_order` (
	`order_id`                BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY, -- идентификатор заказа
	`merchant_id`							BIGINT NOT NULL, -- ID мерчанта
	`merchant_bill_id`				BIGINT NOT NULL, -- ID заказа на вывод в системе мерчанта
  `amount`                  INT NOT NULL, -- сумма заказа
  `currency_alpha`          CHAR(3)	NOT NULL, -- валюта в 3х буквенном коде
  `currency_number`         INT NOT NULL, -- валюта в числовом коде
  `method`                  VARCHAR(64) NOT NULL, -- метод в виде строки
  `method_hash`             BIGINT UNSIGNED NOT NULL, -- хэш строки метода
	`address`                 VARCHAR(64) NOT NULL, -- адрес отправки в виде строки
  `address_hash`            BIGINT UNSIGNED NOT NULL, -- хэш адреса отправки
  `method_config_group`     TINYINT NOT NULL DEFAULT 0, -- группа конфига метода
	`create_date`							DATETIME DEFAULT NULL, -- дата создания
	`close_date`							DATETIME DEFAULT NULL, -- дата закрытия
  `status`                  TINYINT NOT NULL -- статус заказа
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8;
