package template

import (
	"fmt"
	"io"
	"text/template"

	"go.felesatra.moe/felesatra/generator/internal/enja"
)

func Load(dir string) (*template.Template, error) {
	return template.ParseGlob(fmt.Sprintf("%s/*", dir))
}

func RenderEnja(t *template.Template, w io.Writer, d *enja.Document) error {
	h := make(map[string]interface{})
	for k, v := range d.Header {
		h[k] = v
	}
	h["body"] = d.Body
	return t.ExecuteTemplate(w, "site_content.html", h)
}

func RenderEnjaFile(src, dst string) error {
	return nil
}
