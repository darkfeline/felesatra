package templates

import (
	"text/template"
)

//go:generate binpack -name baseTemplate base.html
//go:generate binpack -name indexTemplate index.html

func LoadPageTemplate() *template.Template {
	return template.Must(template.New("base.html").Parse(baseTemplate))
}

func LoadIndexTemplate() *template.Template {
	return template.Must(template.New("index.html").Parse(indexTemplate))
}