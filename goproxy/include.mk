modules_dir := $(srvdir)/goproxy
$(modules_dir):
	mkdir -p $@

.PHONY: mod
clean_paths += goproxy/modules
extraclean_paths += goproxy/repo_cache
mod: goproxy/build_modules.py | $(modules_dir)
	$(PYTHON) $< goproxy/repo_cache $(modules_dir)
