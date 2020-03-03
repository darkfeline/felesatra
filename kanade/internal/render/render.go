package render

import (
	"fmt"
	"io"
	"text/template"

	"kanade/internal/enja"
)

// RenderEnja renders the Enja document using the template.
func RenderEnja(t *template.Template, w io.Writer, d enja.Document) error {
	h := make(map[string]string)
	for k, v := range d.Header {
		h[k] = v
	}
	h["body"] = string(d.Body)
	return t.Execute(w, h)
}

// RenderEnjaFile renders the Enja document in the source file using
// the template.
func RenderEnjaFile(t *template.Template, w io.Writer, src string) error {
	d, err := enja.LoadPath(src)
	if err != nil {
		return fmt.Errorf("render enja file: %s", err)
	}
	if err := RenderEnja(t, w, d); err != nil {
		return fmt.Errorf("render enja file: %s", err)
	}
	return nil
}
