local_dir := www

srv_www_dir := $(srvdir)/www

www_pages_dir := $(local_dir)/pages
www_pages := $(shell find $(www_pages_dir) -name '*.html')

clean_paths += $(local_dir)/index
$(local_dir)/index: $(www_pages) $(kanade_path)
	$(kanade_path) index-pages $(www_pages_dir) > $@

clean_paths += $(local_dir)/index.html
$(local_dir)/index.html: $(local_dir)/index $(kanade_path)
	$(kanade_path) make-index-page $< > $@

clean_paths += $(srv_www_dir)/index.html
all: $(srv_www_dir)/index.html
$(srv_www_dir)/index.html: $(local_dir)/index.html $(kanade_path)
	$(kanade_path) render $< > $@

clean_paths += $(srv_www_dir)/404.html
all: $(srv_www_dir)/404.html
$(srv_www_dir)/404.html: $(local_dir)/pages/404.html $(kanade_path)
	$(kanade_path) render $< > $@

clean_paths += $(srv_www_dir)/atom.xml
all: $(srv_www_dir)/atom.xml
$(srv_www_dir)/atom.xml: $(local_dir)/index $(kanade_path)
	$(kanade_path) make-atom-feed $< > $@

clean_paths += $(srv_www_dir)/sitemap.xml
all: $(srv_www_dir)/sitemap.xml
$(srv_www_dir)/sitemap.xml: $(local_dir)/index $(kanade_path)
	$(kanade_path) make-sitemap $< > $@

include $(local_dir)/render.mk

clean_paths += $(local_dir)/render.mk
$(local_dir)/render.mk: $(local_dir)/index $(kanade_path)
	$(kanade_path) make-render-makefile $< > $@
