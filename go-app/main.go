package main

import (
	"html/template"
	"log"
	"net/http"

	"google.golang.org/appengine"
)

func main() {
	http.HandleFunc("/", handle)
	appengine.Main()
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
    <a href="{{.URL}}">{{$s}}{{.Path}}</a> (<a href="https://godoc.org/{{$s}}{{.Path}}">docs</a>)
  </body>
</html>
`))

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
