package gopkg

import (
	"sync"
	"text/template"
)

// A Spec describes a Go package.
type Spec struct {
	Path    string
	Method  Method
	URL     string
	RepoURL string
}

type Method string

const (
	Mod Method = "mod"
	Git Method = "git"
)

const pageTemplate = `{{$s := "go.felesatra.moe"}}<!DOCTYPE HTML>
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
`

var temp *template.Template
var once sync.Once

func Template() *template.Template {
	once.Do(func() {
		temp = template.Must(template.New("go").Parse(pageTemplate))
	})
	return temp
}
