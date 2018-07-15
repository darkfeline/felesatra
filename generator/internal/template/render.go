package template

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"text/template"

	"github.com/pkg/errors"
	"go.felesatra.moe/felesatra/generator/internal/enja"
)

// Load loads the Template to be used by RenderEnja.
func Load(dir string) (*template.Template, error) {
	return template.ParseGlob(fmt.Sprintf("%s/*", dir))
}

// RenderEnja renders the Enja document using the Template and writes
// it to the Writer.
func RenderEnja(t *template.Template, w io.Writer, d *enja.Document) error {
	h := make(map[string]interface{})
	for k, v := range d.Header {
		h[k] = v
	}
	h["body"] = string(d.Body)
	return t.ExecuteTemplate(w, "site-content.html", h)
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

// ReadEnjaFile reads and returns an Enja document from the file.
// This function also populates missing headers based on the path.
func ReadEnjaFile(path string) (*enja.Document, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	d, err := enja.Decode(f)
	if err != nil {
		return nil, errors.Wrap(err, "decode enja")
	}
	setMissingHeaders(d, path)
	return d, nil
}

const minDate = "0000-00-00"

func setMissingHeaders(d *enja.Document, path string) {
	if _, ok := d.Header["published"]; !ok {
		d.Header["published"] = dateFromPath(path)
	}
	if _, ok := d.Header["modified"]; !ok {
		d.Header["modified"] = d.Header["published"]
	}
}

func dateFromPath(path string) string {
	parts := filepath.SplitList(path)
	if len(parts) < 4 {
		return minDate
	}
	y := parts[len(parts)-4]
	m := parts[len(parts)-3]
	d := parts[len(parts)-2]
	return fmt.Sprintf("%s-%s-%s", y, m, d)
}
