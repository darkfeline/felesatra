.PHONY: all
all: build

SRC_DIR=src
BUILD_DIR=build

.PHONY: build
build:
	python -m felesatra.__main__ ${SRC_DIR} ${BUILD_DIR}

.PHONY: isort
isort:
	isort -rc felesatra

.PHONY: pylint
pylint:
	pylint --output-format=colorized animanager
