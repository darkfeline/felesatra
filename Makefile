srcdir = $(CURDIR)
pagedestdir = app/pages
pagesrcdir = $(srcdir)/pages
srcpages = $(shell find $(pagesrcdir) -type f)
dstpages = $(patsubst $(pagesrcdir)/%,$(pagedestdir)/%,$(srcpages))
felesatra = $(shell find $(srcdir)/felesatra -name __pycache__ -prune , -type f)

PYTHON = PYTHONPATH=$(srcdir) pipenv run python

.PHONY: all
all: $(pages)

.PHONY: deploy
deploy: all
	gcloud app deploy

.PHONY: clean
clean:
	rm -rf $(pagedestdir) page_index

# app/pages
$(subst .html,%,$(dstpages)): $(subst .html,%,$(srcpages)) $(felesatra)
	$(PYTHON) -m felesatra.cmd.render $(pagesrcdir) $(pagedestdir)

page_index: $(srcpages) $(felesatra)
	$(PYTHON) -m felesatra.cmd.index_pages $(pagesrcdir) $@
