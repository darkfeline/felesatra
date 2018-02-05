srcdir = $(CURDIR)
wwwdir = app/www
felesatra = $(shell find $(srcdir)/felesatra -name __pycache__ -prune , -type f)

staticsrcdir = $(srcdir)/static
staticdstdir = $(wwwdir)
staticsrc = $(shell find $(staticsrcdir) -type f)
staticdst = $(patsubst $(staticsrcdir)/%,$(staticdstdir)/%,$(staticsrc))

pagesrcdir = $(srcdir)/pages
pagedstdir = $(wwwdir)
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
	rm -rf $(pagedstdir) page_index

# static
$(staticdst): $(staticdstdir)/%: $(staticsrcdir)/%
	mkdir -p $(dir $@)
	cp $< $@

# pages
$(subst .html,%,$(dstpages)): $(subst .html,%,$(srcpages)) $(felesatra)
	$(PYTHON) -m felesatra.cmd.render $(pagesrcdir) $(pagedstdir)

page_index: $(srcpages) $(felesatra)
	$(PYTHON) -m felesatra.cmd.index_pages $(pagesrcdir) $@
