srcdir = $(CURDIR)
static_files = $(patsubst $(srcdir)/static/%,www/%,$(shell find $(srcdir)/static -type f))
pages = $(patsubst $(srcdir)/pages/%,www/%,$(shell find $(srcdir)/pages -type f))

.PHONY: all
all: $(static_files) $(pages)

.PHONY: deploy
deploy: all
	gcloud app deploy

.PHONY: clean
clean:
	rm -rf www

$(static_files): www/%: static/%
	mkdir -p $(dir $@)
	cp $< $@

$(pages): www/%: pages/%
	mkdir -p $(dir $@)
	PYTHONPATH=$(srcdir) pipenv run python -m felesatra.cmd.render $< $@
