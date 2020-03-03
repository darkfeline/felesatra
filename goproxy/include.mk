local_dir := goproxy

modules_dir := $(srvdir)/goproxy
$(modules_dir):
	mkdir -p $@

.PHONY: mod
clean_paths += $(local_dir)/modules
extraclean_paths += $(local_dir)/repo_cache
mod: local_dir := $(local_dir)
mod: $(local_dir)/build_modules.py | $(modules_dir)
	$(PYTHON) $< $(local_dir)/repo_cache $(modules_dir)
