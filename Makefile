.PHONY: all
all: clean build_local

SRC_DIR=site
BUILD_DIR=build

.PHONY: build
build:
	python -m felesatra ${SRC_DIR} ${BUILD_DIR}

.PHONY: build_local
build_local:
	python -m felesatra --site-url 'http://localhost:5000' ${SRC_DIR} ${BUILD_DIR}

.PHONY: clean
clean:
	rm -rf ${BUILD_DIR}

.PHONY: devserver
devserver:
	python -m devserver ${BUILD_DIR}

.PHONY: test
test:
	nosetests --with-doctest

.PHONY: isort
isort:
	isort -rc felesatra

.PHONY: pylint
pylint:
	pylint --output-format=colorized felesatra
