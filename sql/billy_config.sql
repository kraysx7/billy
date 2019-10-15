-- --------------------------------------------------------
-- Таблица для хранения информации о подключенных магазинах
-- --------------------------------------------------------

DROP TABLE IF EXISTS `billy_merchant`;
CREATE TABLE `billy_merchant` (
    `merchant_id`          BIGINT NOT NULL, -- ID мерчанта
    `name`                 VARCHAR(64) NOT NULL, -- имя
		`role`                 TINYINT NOT NULL DEFAULT 0,
		`tariff_plan`          INT NOT NULL DEFAULT 0, -- тарифный план (?)
		`balance`              INT NOT NULL DEFAULT 0, -- баланс
		`currency_alpha`       CHAR(3) NOT NULL, -- буквенный код валюты из таблицы `iso3166` (устанавливается при регистрации)
		`currency_number`      INT NOT NULL, -- цифровой код валюты из таблицы `iso3166` (устанавливается при регистрации)
		`country_number`       INT NOT NULL, -- цифровой код страны из таблицы `iso3166` (устанавливается при регистрации)

		`ban`                  TINYINT	DEFAULT 0, -- флаг пользователя о бане. если > 0, то юзер заблокирован. значение - причина бана.
		`ban_comment`          VARCHAR(64) DEFAULT NULL, -- ещё более подробное объяснение бана (устанавливается администратором)
		`password`             CHAR(64) NOT NULL, -- md5(...), sha(...), ...

    `reg_date`             VARCHAR(256) NOT NULL, -- дата регистрации
		`email`                VARCHAR(128) DEFAULT NULL, -- почта
		`params`               VARCHAR(2048) DEFAULT NULL, -- описание произвольных параметров пользователя в JSON формате
		`last_login`           DATETIME DEFAULT NULL, -- дата последнего входа в панель управления
		`last_logout`          DATETIME DEFAULT NULL, -- дата последнего выхода в панель управления
		`last_ip`              BIGINT DEFAULT 0, -- ip адрес входа в числовом формате
		`last_ip_alpha`        CHAR(32) DEFAULT NULL, -- ip адрес входа в строковом формате
		`comment`              VARCHAR(64) DEFAULT NULL, -- коментарий к пользователю
		`status`               TINYINT DEFAULT 0, -- статус пользователя (0 - не активирован, 1 - активирован)
		`partner_id`           BIGINT DEFAULT 0 -- id партнёра, который привёл пользователя
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `billy_merchant_config`;
CREATE TABLE `billy_merchant_config` (
    `merchant_id`          BIGINT NOT NULL, -- ID мерчанта
    `key`                  VARCHAR(64) NOT NULL, -- ключ конфига
    `value`                VARCHAR(256) NOT NULL -- значение конфига
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `billy_paysystem`;
CREATE TABLE `billy_paysystem` (
    `paysystem_id`         INT NOT NULL UNIQUE, -- ID системы (считается как 32 битное число, где каждый октет=соотв. букве из ключа)
    `key`                  CHAR(4) NOT NULL UNIQUE, -- 4х буквенный ключ системы
    `name`                 VARCHAR(64) NOT NULL, -- название системы
		`desc`                 VARCHAR(256) DEFAULT NULL -- описание
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `billy_merchant_paysystem`;
CREATE TABLE `billy_merchant_paysystem` (
    `merchant_id`          BIGINT NOT NULL, -- ID мерчанта
    `paysystem_id`         BIGINT NOT NULL, -- ID подключенной системы
    `config`               VARCHAR(2048) NOT NULL, -- настройки для доступа к системе в JSON формате (TODO: Зашифровать закрытым ключом из настроек мерчанта...)
		`activation_status`    INT NOT NULL DEFAULT 0 -- состояние подключения. 0 - выкл, 1 - вкл, 2 - вкл+настроен
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8;
