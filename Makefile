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
DOCKER := sudo docker

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

# Set variables:
# container_registry := gcr.io
# container_region := us-central1
# gcp_project := project-name
# files_bucket := bucket-name
include gcp.mk

.PHONY: upload
upload:
	gsutil rsync -r files gs://$(files_bucket)

.PHONY: remotebuild
remotebuild: app-deps
	cd app && gcloud builds submit --tag $(container_registry)/$(gcp_project)/felesatra

.PHONY: localbuild
localbuild: app-deps
	$(DOCKER) build --tag $(container_registry)/$(gcp_project)/felesatra app
	$(DOCKER) push $(container_registry)/$(gcp_project)/felesatra

.PHONY: deploy
deploy:
	gcloud run deploy felesatra --image $(container_registry)/$(gcp_project)/felesatra \
		--platform managed \
		--region $(container_region) --allow-unauthenticated \
		--memory 128Mi --concurrency 1000 --max-instances 1

.PHONY: remoteclean
remoteclean:
	gcloud container images list-tags $(container_registry)/$(gcp_project)/felesatra \
		--filter='-tags:*'  --format='get(digest)' --limit=50 \
		| xargs -I% gcloud container images delete \
		$(container_registry)/$(gcp_project)/felesatra@% --quiet

.PHONY: bench
.PHONY: test

include kanade/include.mk
include goproxy/include.mk
include www/include.mk
include app/include.mk

# Detect expansion bugs
local_dir := asdfjklasdfjkl
