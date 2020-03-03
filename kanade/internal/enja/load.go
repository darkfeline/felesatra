package enja

import (
	"fmt"
	"os"
	"strings"
)

// LoadPath reads and returns an Enja document from the file.
// This function also populates missing headers based on the path.
func LoadPath(path string) (Document, error) {
	f, err := os.Open(path)
	if err != nil {
		return Document{}, err
	}
	defer f.Close()
	d, err := Decode(f)
	if err != nil {
		return Document{}, fmt.Errorf("read enja file: %w", err)
	}
	setMissingHeaders(&d, path)
	return d, nil
}

func setMissingHeaders(d *Document, path string) {
	if d.Header["published"] == "" {
		if s, ok := dateFromPath(path); ok {
			d.Header["published"] = s
		}
	}
	if d.Header["modified"] == "" {
		if s := d.Header["published"]; s != "" {
			d.Header["modified"] = s
		}
	}
}

func dateFromPath(path string) (date string, ok bool) {
	parts := strings.Split(path, "/")
	if len(parts) < 4 {
		return "", false
	}
	y := parts[len(parts)-4]
	m := parts[len(parts)-3]
	d := parts[len(parts)-2]
	return fmt.Sprintf("%s-%s-%s", y, m, d), true
}
