
-- ----------------------------------------------------
-- Таблица для хранения информации о транзакциях шлюза
-- ----------------------------------------------------

DROP TABLE IF EXISTS `billy_transaction`;
CREATE TABLE `billy_transaction` (
	`transaction_id`          BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY,
  `merchant_id`             BIGINT NOT NULL, -- id мерчанта, которому принадлежит транзакция
	`type`                    INT NOT NULL, -- тип транзакции
  `amount`                  INT NOT NULL DEFAULT 0, -- сумма транзакции
	`ccy_alpha`               CHAR(5)	NOT NULL, -- буквенный код валюты
	`ccy_number`              INT NOT NULL, -- цифровой код валюты
  `params`                  VARCHAR(2048) NOT NULL, -- параметры транзакции в JSON формате
	`create_date`             DATETIME DEFAULT NULL, -- дата открытия
	`close_date`              DATETIME DEFAULT NULL, -- дата закрытия
  `process_result_code`     TINYINT NOT NULL DEFAULT 0, -- код ошибки обработки транзакции
  `ipn_process_date`        DATETIME DEFAULT NULL, -- дата последней попытки оповещения
  `ipn_process_stage`       TINYINT NOT NULL DEFAULT 0, -- текущий этап обработки оповещения транзакции
  `status`                  TINYINT NOT NULL DEFAULT 0	-- статус транзакции. 0 - открыта, 1 = обработана, ожидает оповещения, 2 = закрыта
) ENGINE=InnoDB DEFAULT CHARSET=utf8 AUTO_INCREMENT=1;
