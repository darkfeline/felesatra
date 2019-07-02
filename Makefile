dstdir = www
pagesrcdir = pages
pagedstdir = $(dstdir)/pages
srcpages = $(shell find $(pagesrcdir) -type f)
dstpages = $(patsubst $(pagesrcdir)/%,$(pagedstdir)/%,$(srcpages))

.PHONY: all_quick
all_quick: go_test gen $(dstpages) $(pagedstdir)/index.html $(dstdir)/sitemap.xml $(dstdir)/atom.xml

.PHONY: all_slow
all: build_goproxy all_quick

.PHONY: build_goproxy
clean += goproxy_src goproxy/static
build_goproxy:
	python3 build_goproxy.py goproxy_src goproxy/static

export GO111MODULE=on

.PHONY: go_generate
go_generate:
	cd generator && go generate ./...

.PHONY: go_test
go_test: go_generate
	cd generator && go vet -all ./...
	cd generator && go test ./...

.PHONY: clean
clean:
	rm -rf $(clean)


# Build targets
# generator tool
clean += gen
gen: go_generate $(shell find generator -name "*.go")
	cd generator && go build -o ../gen .

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

# atom.xml
clean += $(dstdir)/atom.xml
$(dstdir)/atom.xml: page_index gen
	./gen atom $< >$@


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
