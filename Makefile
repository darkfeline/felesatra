export PYTHONPATH := $(CURDIR)

PYTHON = python

felesatra_sources = $(shell find felesatra -name __pycache__ -prune , -type f)

dstdir = app
pagesrcdir = pages
pagedstdir = $(dstdir)/pages
srcpages = $(shell find $(pagesrcdir) -type f)
dstpages = $(patsubst $(pagesrcdir)/%,$(pagedstdir)/%,$(srcpages))

.PHONY: all
all: $(dstpages) $(pagedstdir)/index.html $(dstdir)/sitemap.xml

.PHONY: clean
clean:
	rm -rf $(clean)

# Built targets
# Pages
clean += $(pagedstdir)
$(subst .html,%,$(dstpages)): $(subst .html,%,$(srcpages)) $(felesatra_sources)
	$(PYTHON) -m felesatra.cmd.render $(pagesrcdir) $(pagedstdir)

# index.html
$(pagedstdir)/index.html: genpages/index-enja.html $(felesatra_sources)
	$(PYTHON) -m felesatra.cmd.render_single $< $@

# sitemap.xml
clean += sitemap.xml
app/sitemap.xml: page_index $(felesatra_sources)
	$(PYTHON) -m felesatra.cmd.make_sitemap --prefix "https://www.felesatra.moe/" $< $@

# Generated artifacts
# Generated pages
clean += genpages
genpages/index-enja.html: index-template.html page_index $(felesatra_sources)
	mkdir -p $(dir $@)
	$(PYTHON) -m felesatra.cmd.generate_index_page index-template.html page_index >$@

# Page index
clean += page_index
page_index: $(srcpages) $(felesatra_sources)
	$(PYTHON) -m felesatra.cmd.index_pages $(pagesrcdir) $@
