package main

import (
	"html/template"
	"log"
	"net/http"
	"path"
	"strings"

	"google.golang.org/appengine"
)

func main() {
	http.HandleFunc("/", handle)
	appengine.Main()
}

type Method string

const (
	Git Method = "git"
)

type Package struct {
	Path   string
	Method Method
	URL    string
}

var temp = template.Must(template.New("go").Parse(
	`{{$s := "go.felesatra.moe"}}<!DOCTYPE HTML>
<html lang="en">
  <head>
    <meta charset="UTF-8">
    <meta name="go-import" content="{{$s}}{{.Path}} {{.Method}} {{.URL}}">
    <title>{{.Path}}</title>
  </head>
  <body>
    <a href="https://godoc.org/{{$s}}{{.Path}}">{{$s}}{{.Path}}</a>
  </body>
</html>
`))

var pkgs = map[string]*Package{
	"/felesatra": &Package{"/felesatra", Git, "https://github.com/darkfeline/felesatra"},
}

func handle(w http.ResponseWriter, r *http.Request) {
	p, ok := findPackage(r.URL.Path)
	if !ok {
		write404(w)
		return
	}
	err := temp.Execute(w, p)
	if err != nil {
		log.Printf("Error writing response: %s", err)
		serverError(w)
		return
	}
}

func findPackage(pp string) (p *Package, ok bool) {
	for ; pp != "/"; pp, _ = path.Split(pp) {
		pp = strings.TrimRight(pp, "/")
		p, ok := pkgs[pp]
		if ok {
			return p, true
		}
	}
	return nil, false
}

// write404 writes a 404 response.
func write404(w http.ResponseWriter) {
	w.WriteHeader(404)
	_, err := w.Write([]byte("Package not found"))
	if err != nil {
		log.Printf("Error sending server error: %s", err)
	}
}

// serverError writes a generic 500 response.
func serverError(w http.ResponseWriter) {
	w.WriteHeader(500)
	_, err := w.Write([]byte("Error"))
	if err != nil {
		log.Printf("Error sending server error: %s", err)
	}
}
