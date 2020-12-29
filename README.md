# Feles Atra

## Quick Tour

`app` contains the web server source (Go in Docker).

`files` contains the source static files to be served.

`goproxy` contains the script for building modules to serve via
goproxy.

`kanade` is the statis site generator.

`www` contains the HTML source before rendering.

`gcp.mk` contains Makefile variables for GCP:

        container_registry := blah
        container_region := blah
        gcp_project := blah
        files_bucket := blah
