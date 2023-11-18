package gopkg

import (
	_ "embed"
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

//go:embed template.html
var pageTemplate string

var temp *template.Template
var once sync.Once

func Template() *template.Template {
	once.Do(func() {
		temp = template.Must(template.New("go").Parse(pageTemplate))
	})
	return temp
}
