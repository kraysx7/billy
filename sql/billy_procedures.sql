
DELIMITER //

DROP PROCEDURE IF EXISTS `create_transaction`; //
CREATE  PROCEDURE `create_transaction`(
                  IN `in_merchant_id` BIGINT,
                  IN `in_type` INT,
                  IN `in_amount` INT,
                  IN `in_ccy_alpha` CHAR(5),
                  IN `in_ccy_number` INT,
                  IN `in_params` VARCHAR(2048))
BEGIN
	INSERT INTO `billy_transaction` (`merchant_id`, `type`, `amount`, `ccy_alpha`, `ccy_number`, `create_date`, `params`) VALUES
  (`in_merchant_id`, `in_type`, `in_amount`, `in_ccy_alpha`, `in_ccy_number`, NOW(), `in_params`);
	SELECT LAST_INSERT_ID() AS `transaction_id`;
END; //


DROP PROCEDURE IF EXISTS `create_masspayment_order`; //
CREATE  PROCEDURE `create_masspayment_order`(
								IN `in_merchant_id` INT,
								IN `in_merchant_bill_id` INT,
								IN `in_amount` INT,
								IN `in_currency_alpha` CHAR(3),
								IN `in_currency_number` INT,
								IN `in_method` VARCHAR(64),
								IN `in_method_hash` BIGINT UNSIGNED,
								IN `in_address` VARCHAR(64),
								IN `in_address_hash` BIGINT UNSIGNED,
								IN `in_method_config_group` TINYINT,
								IN `in_create_date` DATETIME,
								IN `in_status` TINYINT)
BEGIN
	INSERT INTO `billy_masspayment_order` (`merchant_id`, `merchant_bill_id`, `amount`, `currency_alpha`, `currency_number`, `method`, `method_hash`, `address`, `address_hash`, `method_config_group`, `create_date`, `status`) VALUES
	 (`in_merchant_id`, `in_merchant_bill_id`, `in_amount`, `in_currency_alpha`, `in_currency_number`, `in_method`, `in_method_hash`, `in_address`, `in_address_hash`, `in_method_config_group`, `in_create_date`, `in_status`);

	SELECT LAST_INSERT_ID() AS `order_id`;
END; //

DELIMITER ;
