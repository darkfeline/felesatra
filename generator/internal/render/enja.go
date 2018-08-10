package render

import (
	"fmt"
	"os"
	"strings"

	"github.com/pkg/errors"
	"go.felesatra.moe/felesatra/generator/internal/enja"
)

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
	parts := strings.Split(path, "/")
	if len(parts) < 4 {
		return minDate
	}
	y := parts[len(parts)-4]
	m := parts[len(parts)-3]
	d := parts[len(parts)-2]
	return fmt.Sprintf("%s-%s-%s", y, m, d)
}
