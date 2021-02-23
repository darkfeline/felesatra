package templates

import (
	"embed"
	"html/template"
	"kanade/internal/index"
)

//go:embed *.html
var f embed.FS

var BaseTemplate = template.Must(template.ParseFS(f, "base.html"))

type BaseData struct {
	Title     string
	Published string
	Modified  string
	Body      template.HTML
}

var IndexTemplate = template.Must(template.ParseFS(f, "index.html"))

type IndexData struct {
	Pages []index.Entry
}
