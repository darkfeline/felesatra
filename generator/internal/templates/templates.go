package templates

import (
	"text/template"
)

//go:generate binpack -var baseTemplate base.html
//go:generate binpack -var sitemapTemplate sitemap.xml
//go:generate binpack -var indexTemplate index.html

func LoadPageTemplate() *template.Template {
	return template.Must(template.New("base.html").Parse(baseTemplate))
}

func LoadIndexTemplate() *template.Template {
	return template.Must(template.New("index.html").Parse(indexTemplate))
}

func LoadSitemapTemplate() *template.Template {
	return template.Must(template.New("sitemap.xml").Parse(sitemapTemplate))
}
