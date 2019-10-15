#  File:	 GNU Makefile
#  Author:	 Ilya Troshkov
#  Created:	 08.05.2014 13:14:34

VSN = 1.1

CBSYSTEM_ROOT_DIR=/usr/local/lib/cbsystem
CBSYSTEM_SRC_DIR=/usr/local/src/cbsystem

APP_NAME=billy
APP_RELATIVE_NAME=billy
INSTALL_DIR=${CBSYSTEM_ROOT_DIR}/${APP_RELATIVE_NAME}
SRC_DIR=${CBSYSTEM_SRC_DIR}/${APP_RELATIVE_NAME}

install:
	echo -n ${INSTALL_DIR}
	@[ -n "${INSTALL_DIR}" ] || (echo "Set INSTALL_DIR before running the install target."; false)

	rsync -rupE _build/default/lib/ ${INSTALL_DIR}

	rsync -rupE priv $(INSTALL_DIR)/$(APP_NAME)
	rsync -rupE include $(INSTALL_DIR)/$(APP_NAME)

	cp $(APP_NAME).config $(INSTALL_DIR)/$(APP_NAME).config


install-dev:
	echo -n ${INSTALL_DIR}
	@[ -n "${INSTALL_DIR}" ] || (echo "Set INSTALL_DIR before running the install target."; false)

	rsync -rupE _build/default/lib/ ${INSTALL_DIR}

	rm -rf $(INSTALL_DIR)/$(APP_RELATIVE_NAME)/priv
	ln -s $(SRC_DIR)/priv $(INSTALL_DIR)/$(APP_RELATIVE_NAME)/priv
	rsync -rupE include $(INSTALL_DIR)/$(APP_RELATIVE_NAME)

echo-version:
	@echo $(VSN)
