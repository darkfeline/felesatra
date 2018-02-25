srcdir = $(CURDIR)

PYTHON = PYTHONPATH=$(srcdir) pipenv run python

felesatra_sources = $(shell find $(srcdir)/felesatra -name __pycache__ -prune , -type f)

pagesrcdir = $(srcdir)/pages
pagedstdir = app/pages
srcpages = $(shell find $(pagesrcdir) -type f)
dstpages = $(patsubst $(pagesrcdir)/%,$(pagedstdir)/%,$(srcpages))

.PHONY: all
all: $(dstpages) sitemap.xml

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

clean += page_index
page_index: $(srcpages) $(felesatra_sources)
	$(PYTHON) -m felesatra.cmd.index_pages $(pagesrcdir) $@

clean += sitemap.xml
sitemap.xml: page_index $(felesatra_sources)
	$(PYTHON) -m felesatra.cmd.make_sitemap --prefix "https://www.felesatra.moe/" $< $@
