local_dir := app

app_deps :=
app_deps += $(local_dir)/config.go

app_deps += $(local_dir)/gsconfig.go
$(local_dir)/gsconfig.go: $(local_dir)/make_gsconfig.sh
	sh $< $(files_bucket) >$@

.PHONY: test-app
test: test-app
test-app: local_dir := $(local_dir)
test-app: $(app_deps)
	cd $(local_dir) && $(GO) test ./...

.PHONY: bench-app
bench: bench-app
bench-app: local_dir := $(local_dir)
bench-app: $(app_deps)
	cd $(local_dir) && $(GO) test -bench . ./...
