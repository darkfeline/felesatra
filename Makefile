# make all          Make everything except goproxy modules
#
# make push         Make everything, build, deploy, and clean up
#
# make mod          Build goproxy modules
# make upload       Upload files to Google Storage
# make remotebuild  Do remote build of container
# make localbuild   Do local build of container and upload
# make deploy       Deploy tagged container in registry
# make remoteclean  Delete remote container images
#
# make test         Run tests
# make bench        Run benchmarks
# make clean        Clean up
# make extraclean   Also delete goproxy cache

PYTHON := python3
GO := go
DOCKER := podman

srvdir := app/srv
clean_paths :=
extraclean_paths :=

.PHONY: all
all:

.PHONY: push
push: all mod localbuild deploy remoteclean

.PHONY: clean
clean:
	rm -rf $(clean_paths)
	find . -name "__pycache__" -print0 | xargs -0 rm -rf
	find * -depth -type d -exec rmdir --ignore-fail-on-non-empty {} \+

.PHONY: extraclean
extraclean: clean
	rm -rf $(extraclean_paths)

include private/include.mk

.PHONY: upload
upload:
	gsutil rsync -r files gs://$(files_bucket)

.PHONY: remotebuild
remotebuild: $(app_deps)
	cd app && gcloud builds submit --tag $(container_image)

.PHONY: localbuild
localbuild: $(app_deps)
	$(DOCKER) build --tag $(container_image) --format docker app
	$(DOCKER) push $(container_image)

.PHONY: deploy
deploy:
	gcloud run deploy felesatra --image $(container_image) \
		--platform managed --region $(container_region) \
		--service-account $(service_account) \
		--allow-unauthenticated --memory 128Mi --concurrency 1000 --max-instances 1

.PHONY: remoteclean
remoteclean:
	gcloud container images list-tags $(container_image) \
		--filter='-tags:*'  --format='get(digest)' --limit=50 \
		| xargs -I% gcloud container images delete \
		$(container_image)@% --quiet

.PHONY: bench
.PHONY: test

include kanade/include.mk
include goproxy/include.mk
include www/include.mk
include app/include.mk

# Detect expansion bugs
local_dir := asdfjklasdfjkl
