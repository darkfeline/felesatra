package main

import (
	"html/template"
	"path/filepath"
)

var templatesDir = "templates"
var templ = template.Must(template.ParseGlob(filepath.Join(templatesDir, "*")))
