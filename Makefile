srcdir = $(CURDIR)
pagedestdir = app/pages
pagesrcdir = $(srcdir)/pages
pages = $(patsubst $(pagesrcdir)/%,$(pagedestdir)/%,$(shell find $(pagesrcdir) -type f))
felesatra = $(shell find $(srcdir)/felesatra -name __pycache__ -prune , -type f)

PYTHON = PYTHONPATH=$(srcdir) pipenv run python

.PHONY: all
all: $(pages)

.PHONY: deploy
deploy: all
	gcloud app deploy

.PHONY: clean
clean:
	rm -rf $(pagedestdir)

$(subst .html,%,$(pages)): $(subst .html,%,$(patsubst $(pagedestdir)%,$(pagesrcdir)%,$(pages))) $(felesatra)
	$(PYTHON) -m felesatra.cmd.render $(pagesrcdir) $(pagedestdir)

# $(pages): $(pagedestdir)/%: $(pagesrcdir)/% $(felesatra)
# 	mkdir -p $(dir $@)
# 	$(PYTHON) -m felesatra.cmd.render $< $@
