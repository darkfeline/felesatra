package template

import (
	"fmt"
	"io"
	"os"
	"text/template"

	"github.com/pkg/errors"
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
	h["body"] = string(d.Body)
	return t.ExecuteTemplate(w, "site-content.html", h)
}

func RenderEnjaFile(t *template.Template, src, dst string) error {
	f, err := os.Open(src)
	if err != nil {
		return err
	}
	defer f.Close()
	d, err := enja.Decode(f)
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
