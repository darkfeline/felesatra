local_dir := app

.PHONY: test-app
test: test-app
test-app: local_dir := $(local_dir)
test-app:
	cd $(local_dir) && $(GO) test ./...
