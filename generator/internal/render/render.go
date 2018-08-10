package render

import (
	"io"
	"os"
	"text/template"

	"github.com/pkg/errors"
	"go.felesatra.moe/felesatra/generator/internal/enja"
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

func RenderEnjaFile(t *template.Template, src, dst string) error {
	d, err := ReadEnjaFile(src)
	if err != nil {
		return errors.Wrap(err, "decode enja")
	}
	f2, err := os.Create(dst)
	if err != nil {
		return err
	}
	defer f2.Close()
	if err := RenderEnja(t, f2, d); err != nil {
		return errors.Wrap(err, "render enja")
	}
	if err := f2.Close(); err != nil {
		return err
	}
	return nil
}
