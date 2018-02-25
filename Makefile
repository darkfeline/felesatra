srcdir = $(CURDIR)

PYTHON = env PYTHONPATH=$(srcdir) python

felesatra_sources = $(shell find $(srcdir)/felesatra -name __pycache__ -prune , -type f)

pagesrcdir = $(srcdir)/pages
pagedstdir = app/pages
srcpages = $(shell find $(pagesrcdir) -type f)
dstpages = $(patsubst $(pagesrcdir)/%,$(pagedstdir)/%,$(srcpages))

.PHONY: all
all: $(dstpages) $(pagedstdir)/index.html sitemap.xml

.PHONY: deploy
deploy: all
	gcloud app deploy

.PHONY: clean
clean:
	rm -rf $(clean)

# pages
clean += $(pagedstdir)
$(subst .html,%,$(dstpages)): $(subst .html,%,$(srcpages)) $(felesatra_sources)
	$(PYTHON) -m felesatra.cmd.render $(pagesrcdir) $(pagedstdir)

$(pagedstdir)/index.html: genpages/index-enja.html $(felesatra_sources)
	$(PYTHON) -m felesatra.cmd.render_single $< $@

clean += genpages
genpages/index-enja.html: index-template.html page_index $(felesatra_sources)
	mkdir -p $(dir $@)
	$(PYTHON) -m felesatra.cmd.generate_index_page index-template.html page_index >$@

clean += sitemap.xml
sitemap.xml: page_index $(felesatra_sources)
	$(PYTHON) -m felesatra.cmd.make_sitemap --prefix "https://www.felesatra.moe/" $< $@

clean += page_index
page_index: $(srcpages) $(felesatra_sources)
	$(PYTHON) -m felesatra.cmd.index_pages $(pagesrcdir) $@
