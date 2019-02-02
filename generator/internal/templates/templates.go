package templates

import (
	"path/filepath"
	"text/template"
)

type Templates struct {
	Page    *template.Template
	Index   *template.Template
	Sitemap *template.Template
}

func LoadPageTemplate(dir string) (*template.Template, error) {
	return template.ParseFiles(filepath.Join(dir, "base.html"))
}

func LoadIndexTemplate(dir string) (*template.Template, error) {
	return template.ParseFiles(filepath.Join(dir, "index.html"))
}

func LoadSitemapTemplate(dir string) (*template.Template, error) {
	return template.ParseFiles(filepath.Join(dir, "sitemap.xml"))
}
