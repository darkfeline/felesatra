local_dir := kanade

kanade_path := $(local_dir)/kanade

kanade_templates := $(shell find $(local_dir) -name '*.html')

clean_paths += $(kanade_path)
$(kanade_path): local_dir := $(local_dir)
$(kanade_path): $(local_dir)/go.mod $(kanade_templates) \
		$(shell find $(local_dir) -name '*.go' ! -name '*_test.go')
	cd $(local_dir) && $(GO) build -o kanade

.PHONY: test-kanade
test: test-kanade
test-kanade: local_dir := $(local_dir)
test-kanade:
	cd $(local_dir) && $(GO) test ./...
