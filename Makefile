export PYTHONPATH := $(CURDIR)

PYTHON = python

felesatra_sources = $(shell find felesatra -name __pycache__ -prune , -type f)

dstdir = app
pagesrcdir = pages
pagedstdir = $(dstdir)/pages
srcpages = $(shell find $(pagesrcdir) -type f)
dstpages = $(patsubst $(pagesrcdir)/%,$(pagedstdir)/%,$(srcpages))
templates = $(shell find templates -type f)

.PHONY: all
all: gentest gen $(dstpages) $(pagedstdir)/index.html $(dstdir)/sitemap.xml

.PHONY: gentest
gentest:
	go vet ./generator/...
	go test ./generator/...

clean += gen
gen: $(shell find generator -name "*.go")
	go build -o gen ./generator

.PHONY: clean
clean:
	rm -rf $(clean)

# Built targets
# Pages
clean += $(pagedstdir)
$(subst .html,%,$(dstpages)): $(subst .html,%,$(srcpages)) gen $(templates)
	./gen rendermany $(pagesrcdir) $(pagedstdir)

# index.html
$(pagedstdir)/index.html: genpages/index-enja.html gen $(templates)
	./gen render $< $@

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
