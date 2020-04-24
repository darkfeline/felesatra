package templates

import (
	"html/template"
	"kanade/internal/index"
)

//go:generate binpack -name baseText base.html

var BaseTemplate = template.Must(template.New("base").Parse(baseText))

type BaseData struct {
	Title     string
	Published string
	Modified  string
	Body      template.HTML
}

//go:generate binpack -name indexText index.html

var IndexTemplate = template.Must(template.New("index").Parse(indexText))

type IndexData struct {
	Pages []index.Entry
}
