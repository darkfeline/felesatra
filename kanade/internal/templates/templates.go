package templates

import (
	"text/template"
)

//go:generate binpack -name baseText base.html

var BaseTemplate = template.Must(template.New("base").Parse(baseText))

//go:generate binpack -name indexText index.html

var IndexTemplate = template.Must(template.New("index").Parse(indexText))
