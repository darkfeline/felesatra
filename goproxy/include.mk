modules_dir := $(srvdir)/goproxy
clean_paths += $(modules_dir)
$(modules_dir):
	mkdir -p $@

.PHONY: mod
clean_paths += goproxy/modules
extraclean_paths += goproxy/repo_cache
mod: goproxy/goproxy | $(modules_dir)
	$< goproxy/repo_cache $(modules_dir)

clean_paths += goproxy/goproxy
goproxy/goproxy: goproxy/go.mod \
		$(shell find goproxy -name '*.go' ! -name '*_test.go')
	cd goproxy && $(GO) build -o goproxy

.PHONY: test-goproxy
test: test-goproxy
test-goproxy:
	cs goproxy && $(GO) test ./...
