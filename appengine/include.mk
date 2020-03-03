local_dir := appengine

.PHONY: test-appengine
test: test-appengine
test-appengine: local_dir := $(local_dir)
test-appengine:
	cd $(local_dir) && $(GO) test ./...
