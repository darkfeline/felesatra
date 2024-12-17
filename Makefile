# make all          Make everything except goproxy modules
#
# make push         Make everything, build, deploy, and clean up
#
# make mod          Build goproxy modules
# make localbuild   Do local build of container and upload
# make remotebuild  Do remote build of container
# make upload       Upload files to Google Storage
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
include private/config.mk

include goproxy/include.mk
include kanade/include.mk
include www/include.mk
include app/include.mk
include private/include.mk
include quartz/include.mk

.PHONY: all
all:

.PHONY: push
.PHONY: quartz-copy
push: all quartz-copy mod localbuild deploy remoteclean

.PHONY: localbuild
localbuild: app
	$(DOCKER) build --tag $(container_image) --format docker app
	$(DOCKER) push $(container_image)

.PHONY: remotebuild
remotebuild: app
	cd app && gcloud builds submit --tag $(container_image)

.PHONY: upload
upload:
	gsutil rsync -r files gs://$(files_bucket)

.PHONY: deploy
deploy:
	gcloud run deploy felesatra --project $(gcp_project) \
                --image $(container_image) \
		--platform managed --region $(container_region) \
		--service-account $(service_account) \
		--allow-unauthenticated --memory 128Mi --concurrency 1000 --max-instances 1

.PHONY: remoteclean
remoteclean:
	gcloud container images list-tags $(container_image) \
		--filter='-tags:*'  --format='get(digest)' --limit=50 \
		| xargs -I% gcloud container images delete \
		$(container_image)@% --quiet

.PHONY: test
.PHONY: bench

.PHONY: clean
clean:
	rm -rf $(clean_paths)
	find . -name "__pycache__" -print0 | xargs -0 rm -rf

.PHONY: extraclean
extraclean: clean
	rm -rf $(extraclean_paths)
