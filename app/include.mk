local_dir := app

.PHONY: test-app
test: test-app
test-app: local_dir := $(local_dir)
test-app:
	cd $(local_dir) && $(GO) test ./...

.PHONY: bench-app
bench: bench-app
bench-app: local_dir := $(local_dir)
bench-app:
	cd $(local_dir) && $(GO) test -bench . ./...
