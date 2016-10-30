.PHONY: all
all: clean build_local

BUILD_DIR=build
LOCAL_BUILD_DIR=build_local

.PHONY: build
build:
	python -m felesatra ${BUILD_DIR}

SSH_HOST=www.felesatra.moe
SSH_PORT=22
SSH_USER=root
SSH_DIR=/srv/www/www.felesatra.moe

.PHONY: upload
upload: clean build
	rsync -e "ssh -p $(SSH_PORT)" -P -rvzc --delete $(BUILD_DIR)/ $(SSH_USER)@$(SSH_HOST):$(SSH_DIR) --cvs-exclude

.PHONY: build_local
build_local:
	python -m felesatra --site-url 'http://localhost:5000' ${LOCAL_BUILD_DIR}

.PHONY: clean
clean:
	rm -rf ${BUILD_DIR}
	rm -rf ${LOCAL_BUILD_DIR}

.PHONY: devserver
devserver:
	python -m devserver ${LOCAL_BUILD_DIR}

.PHONY: watch
watch:
	bin/watch

.PHONY: test
test:
	nosetests --with-doctest

.PHONY: isort
isort:
	isort -rc felesatra

.PHONY: pylint
pylint:
	pylint --output-format=colorized felesatra tests
