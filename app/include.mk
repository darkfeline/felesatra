local_dir := app

app_deps += $(local_dir)/gsconfig.go
$(local_dir)/gsconfig.go: $(local_dir)/make_gsconfig.sh gcp.mk
	sh $< $(files_bucket) >$@

# Generate default template if missing.
app_deps += $(local_dir)/auth.go
$(local_dir)/auth.go: local_dir := $(local_dir)
# DO NOT ADD DEP TO THIS RULE.
$(local_dir)/auth.go:
	[ -f $@ ] || sh $(local_dir)/make_auth.sh >$@

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
