www_pages_dir := www/pages
www_pages := $(shell find $(www_pages_dir) -name '*.html')

clean_paths += www/index
www/index: $(www_pages) $(kanade_path)
	$(kanade_path) index-pages $(www_pages_dir) > $@

clean_paths += www/index.html
www/index.html: www/index $(kanade_path)
	$(kanade_path) make-index-page $< > $@

clean_paths += $(srvdir)/www/index.html
all: $(srvdir)/www/index.html
$(srvdir)/www/index.html: www/index.html $(kanade_path)
	$(kanade_path) render $< > $@

clean_paths += $(srvdir)/www/404.html
all: $(srvdir)/www/404.html
$(srvdir)/www/404.html: www/pages/404.html $(kanade_path)
	$(kanade_path) render $< > $@

clean_paths += $(srvdir)/www/atom.xml
all: $(srvdir)/www/atom.xml
$(srvdir)/www/atom.xml: www/index $(kanade_path)
	$(kanade_path) make-atom-feed $< > $@

clean_paths += $(srvdir)/www/sitemap.xml
all: $(srvdir)/www/sitemap.xml
$(srvdir)/www/sitemap.xml: www/index $(kanade_path)
	$(kanade_path) make-sitemap $< > $@

include www/render.mk
clean_paths += www/render.mk
www/render.mk: www/index $(kanade_path)
	$(kanade_path) make-render-makefile $< > $@
