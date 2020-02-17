package main

import (
	"fmt"
	"html/template"
	"log"
	"net/http"
	"os"
	"path"
	"strings"
)

func main() {
	port := os.Getenv("PORT")
	if port == "" {
		port = "8080"
		log.Printf("Defaulting to port %s", port)
	}
	log.Printf("Listening on port %s", port)
	http.HandleFunc("/", handle)
	log.Fatal(http.ListenAndServe(fmt.Sprintf(":%s", port), nil))
}

var pageTemplate = template.Must(template.New("go").Parse(
	`{{$s := "go.felesatra.moe"}}<!DOCTYPE HTML>
<html lang="en">
  <head>
    <meta charset="UTF-8">
    <meta name="go-import" content="{{$s}}{{.Path}} {{.Method}} {{.URL}}">
    <title>{{.Path}}</title>
  </head>
  <body>
    <a href="{{.RepoURL}}">{{$s}}{{.Path}}</a> (<a href="https://pkg.go.dev/{{$s}}{{.Path}}">docs</a>)
  </body>
</html>
`))

func handle(w http.ResponseWriter, r *http.Request) {
	p, ok := findPackage(r.URL.Path)
	if !ok {
		w.WriteHeader(404)
		w.Write([]byte("Package not found"))
		return
	}
	w.Header()["Cache-Control"] = []string{"public,max-age=604800"}
	pageTemplate.Execute(w, p)
}

// findPackage finds the longest matching package prefix for the given path.
func findPackage(pp string) (p *pkgSpec, ok bool) {
	for ; pp != "/"; pp, _ = path.Split(pp) {
		pp = strings.TrimRight(pp, "/")
		p, ok := pkgMap[pp]
		if ok {
			return p, true
		}
	}
	return nil, false
}
