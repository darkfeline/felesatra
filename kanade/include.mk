kanade_path := kanade/kanade

clean_paths += $(kanade_path)
$(kanade_path): kanade/go.mod \
		$(shell find kanade -name '*.html') \
		$(shell find kanade -name '*.go' ! -name '*_test.go')
	cd kanade && $(GO) build -o kanade

.PHONY: test-kanade
test: test-kanade
test-kanade:
	cd kanade && $(GO) test ./...
