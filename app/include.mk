.PHONY: app
all: app
app: app/config.go

clean_paths += app/config.go
app/config.go: private/config.go
	cp $< $@

app: app/gsconfig.go
clean_paths += app/gsconfig.go
app/gsconfig.go: app/make_gsconfig.sh
	sh $< $(files_bucket) >$@

.PHONY: test-app
test: test-app
test-app: app
	cd app && $(GO) test ./...

.PHONY: bench-app
bench: bench-app
bench-app: app
	cd app && $(GO) test -bench . ./...
