# Feles Atra

## Quick Tour

`app` contains the web server source (Go in Docker).

`files` contains the source static files to be served.

`goproxy` contains the script for building modules to serve via
goproxy.

`kanade` is the static site generator.

`www` contains the HTML source before rendering.

`identity` helps generate the signed identity text file.  This gets
moved manually into `srv/www` and committed.
