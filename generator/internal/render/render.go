package render

import (
	"fmt"
	"io"
	"os"
	"text/template"

	"generator/internal/enja"
)

// RenderEnja renders the Enja document using the Template and writes
// it to the Writer.
func RenderEnja(t *template.Template, w io.Writer, d *enja.Document) error {
	h := make(map[string]interface{})
	for k, v := range d.Header {
		h[k] = v
	}
	h["body"] = string(d.Body)
	return t.Execute(w, h)
}

// RenderEnjaFile renders the Enja document in the source file using
// the template and writes it to the dest file.
func RenderEnjaFile(t *template.Template, src, dst string) error {
	d, err := enja.LoadPath(src)
	if err != nil {
		return fmt.Errorf("render enja file: %w", err)
	}
	f, err := os.Create(dst)
	if err != nil {
		return err
	}
	defer f.Close()
	if err := RenderEnja(t, f, d); err != nil {
		return fmt.Errorf("render enja file: %w", err)
	}
	if err := f.Close(); err != nil {
		return fmt.Errorf("render enja file: %w", err)
	}
	return nil
}
