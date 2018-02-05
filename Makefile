srcdir = $(CURDIR)
felesatra = $(shell find $(srcdir)/felesatra -name __pycache__ -prune , -type f)

pagesrcdir = $(srcdir)/pages
pagedstdir = app/pages
srcpages = $(shell find $(pagesrcdir) -type f)
dstpages = $(patsubst $(pagesrcdir)/%,$(pagedstdir)/%,$(srcpages))

PYTHON = PYTHONPATH=$(srcdir) pipenv run python

.PHONY: all
all: $(dstpages) $(staticdst)

.PHONY: deploy
deploy: all
	gcloud app deploy

.PHONY: clean
clean:
	rm -rf $(clean)

# pages
clean += $(pagedstdir)
$(subst .html,%,$(dstpages)): $(subst .html,%,$(srcpages)) $(felesatra)
	$(PYTHON) -m felesatra.cmd.render $(pagesrcdir) $(pagedstdir)

clean += page_index
page_index: $(srcpages) $(felesatra)
	$(PYTHON) -m felesatra.cmd.index_pages $(pagesrcdir) $@
