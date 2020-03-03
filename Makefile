# make all         Make everything
# make mod         Build goproxy modules
# make clean       Clean up
# make extraclean  Also delete goproxy cache

PYTHON := python3
GO := go

srvdir := appengine/srv
clean_paths :=
extraclean_paths :=

.PHONY: all
all:

.PHONY: clean
clean:
	rm -rf $(clean_paths)
	find . -name "__pycache__" -print0 | xargs -0 rm -rf
	find . -depth -type d -exec rmdir --ignore-fail-on-non-empty {} \+

.PHONY: extraclean
extraclean: clean
	rm -rf $(extraclean_paths)

.PHONY: deploy
deploy:
	cd appengine && gcloud app deploy --quiet
	bash poke_proxy.sh

.PHONY: test

include kanade/include.mk
include goproxy/include.mk
include www/include.mk
include appengine/include.mk

# Detect expansion bugs
local_dir := asdfjklasdfjkl
