.PHONY: all
all: clean build_local

SRC_DIR=site
BUILD_DIR=build
LOCAL_BUILD_DIR=build_local

.PHONY: build
build:
	python -m felesatra ${SRC_DIR} ${BUILD_DIR}

.PHONY: build_local
build_local:
	python -m felesatra --site-url 'http://localhost:5000' ${SRC_DIR} ${LOCAL_BUILD_DIR}

.PHONY: clean
clean:
	rm -rf ${BUILD_DIR}
	rm -rf ${LOCAL_BUILD_DIR}

.PHONY: devserver
devserver:
	python -m devserver ${LOCAL_BUILD_DIR}

.PHONY: test
test:
	nosetests --with-doctest

.PHONY: isort
isort:
	isort -rc felesatra

.PHONY: pylint
pylint:
	pylint --output-format=colorized felesatra
