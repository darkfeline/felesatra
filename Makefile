srcdir = $(CURDIR)
pagedestdir = app/pages
pagesrcdir = $(srcdir)/pages
pages = $(patsubst $(pagesrcdir)/%,$(pagedestdir)/%,$(shell find $(pagesrcdir) -type f))

PYTHON = PYTHONPATH=$(srcdir) pipenv run python

.PHONY: all
all: $(static_files) $(pages)

.PHONY: deploy
deploy: all
	gcloud app deploy

.PHONY: clean
clean:
	rm -rf $(pagedestdir)

$(pages): $(pagedestdir)/%: $(pagesrcdir)/%
	mkdir -p $(dir $@)
	$(PYTHON) -m felesatra.cmd.render $< $@
