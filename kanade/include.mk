local_dir := kanade

kanade_path := $(local_dir)/kanade

kanade_template_dir := $(local_dir)/internal/templates
kanade_templates := $(addprefix $(kanade_template_dir)/,base.html index.html)

clean_paths += $(kanade_path)
$(kanade_path): local_dir := $(local_dir)
$(kanade_path): $(local_dir)/go.mod $(kanade_templates) \
		$(shell find $(local_dir) -name '*.go' ! -name '*_test.go')
	cd $(local_dir) && $(GO) build -o kanade

.PHONY: test-kanade
test: test-kanade
test-kanade: local_dir := $(local_dir)
test-kanade: $(kanade_templates)
	cd $(local_dir) && $(GO) test ./...
