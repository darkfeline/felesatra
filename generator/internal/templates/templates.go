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
	t, err := template.ParseFiles(filepath.Join(dir, "base.html"))
	if err != nil {
		return nil, err
	}
	_, err = t.ParseGlob(filepath.Join(dir, "site", "*"))
	if err != nil {
		return nil, err
	}
	return t, nil
}

func LoadIndexTemplate(dir string) (*template.Template, error) {
	return template.ParseFiles(filepath.Join(dir, "index.html"))
}

func LoadSitemapTemplate(dir string) (*template.Template, error) {
	return template.ParseFiles(filepath.Join(dir, "sitemap.xml"))
}
