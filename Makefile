dstdir = www
pagesrcdir = pages
pagedstdir = $(dstdir)/pages
srcpages = $(shell find $(pagesrcdir) -type f)
dstpages = $(patsubst $(pagesrcdir)/%,$(pagedstdir)/%,$(srcpages))

.PHONY: all
all: go_test gen $(dstpages) $(pagedstdir)/index.html $(dstdir)/sitemap.xml

.PHONY: go_generate
go_generate:
	go generate ./generator/...

.PHONY: go_test
go_test: go_generate
	go vet -all ./generator/...
	go test ./generator/...

.PHONY: clean
clean:
	rm -rf $(clean)


# Build targets
# generator tool
clean += gen
gen: go_generate $(shell find generator -name "*.go")
	go build -o gen ./generator

# Pages
clean += $(pagedstdir)
$(subst .html,%,$(dstpages)): $(subst .html,%,$(srcpages)) gen
	./gen rendermany $(pagesrcdir) $(pagedstdir)

# index.html
clean += $(pagedstdir)/index.html
$(pagedstdir)/index.html: genpages/index-enja.html gen
	./gen render $< $@

# sitemap.xml
clean += $(dstdir)/sitemap.xml
$(dstdir)/sitemap.xml: page_index gen
	./gen sitemap $< >$@


# Generated artifacts
# Generated pages
clean += genpages
genpages/index-enja.html: page_index gen
	mkdir -p $(dir $@)
	./gen indexpage $< >$@

# Page index
clean += page_index
page_index: $(srcpages) gen
	./gen index $@ $(pagesrcdir)
