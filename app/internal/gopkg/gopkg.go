package gopkg

import (
	_ "embed"
	"html/template"
	"sync"
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

// Template returns the template for formatting a Go package page.
var Template = sync.OnceValue(func() *template.Template {
	return template.Must(template.New("go").Parse(pageTemplate))
})
