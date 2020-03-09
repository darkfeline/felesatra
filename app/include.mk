local_dir := app

$(local_dir)/gsconfig.go: $(local_dir)/make_gsconfig.sh gcp.mk
	bash $< $(files_bucket) > $@

.PHONY: app-deps
all: app-deps
app-deps: $(local_dir)/gsconfig.go

.PHONY: test-app
test: test-app
test-app: local_dir := $(local_dir)
test-app: app-deps
	cd $(local_dir) && $(GO) test ./...

.PHONY: bench-app
bench: bench-app
bench-app: local_dir := $(local_dir)
bench-app: app-deps
	cd $(local_dir) && $(GO) test -bench . ./...
