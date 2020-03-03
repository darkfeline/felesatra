// Package index provides page indexing structures.
package index

import (
	"encoding/csv"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"

	"kanade/internal/enja"
)

// Entry is an index entry.
type Entry struct {
	Path      string
	Title     string
	Published string
	Modified  string
}

// IndexDir creates index entries for the enja documents in a
// directory recursively.
func IndexDir(dir string) ([]Entry, error) {
	dir = filepath.Clean(dir)
	var e []Entry
	wf := func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if info.IsDir() {
			return nil
		}
		relpath := path[len(dir)+1:]
		if isIgnoredFile(relpath) {
			return nil
		}
		d, err := enja.LoadPath(path)
		if err != nil {
			return fmt.Errorf("index dir: load %s: %w", path, err)
		}
		en, err := docEntry(relpath, d)
		if err != nil {
			return fmt.Errorf("index dir: make entry for %s: %w", path, err)
		}
		e = append(e, en)
		return nil
	}
	if err := filepath.Walk(dir, wf); err != nil {
		return nil, err
	}
	return e, nil
}

func isIgnoredFile(path string) bool {
	switch {
	case path == "index.html":
		return true
	case path == "404.html":
		return true
	case path[len(path)-1] == '~':
		return true
	case path[0] == '.':
		return true
	default:
		return false
	}
}

// docEntry creates an index entry for the document.
// The path should be relative to the root of the website.
func docEntry(path string, d enja.Document) (e Entry, err error) {
	ext := filepath.Ext(path)
	e.Path = path[:len(path)-len(ext)]
	if v := d.Header["title"]; v != "" {
		e.Title = v
	} else {
		return e, errors.New("missing title")
	}
	if v := d.Header["published"]; v != "" {
		e.Published = v
	}
	if v := d.Header["modified"]; v != "" {
		e.Modified = v
	}
	return e, nil
}

func ReadAll(r io.Reader) ([]Entry, error) {
	var e []Entry
	cr := csv.NewReader(r)
	cr.ReuseRecord = true
	for {
		row, err := cr.Read()
		switch err {
		case io.EOF:
			return e, nil
		case nil:
			e = append(e, Entry{
				Path:      row[0],
				Title:     row[1],
				Published: row[2],
				Modified:  row[3],
			})
		default:
			return nil, err
		}
	}
}

func WriteAll(w io.Writer, e []Entry) error {
	cw := csv.NewWriter(w)
	row := make([]string, 4)
	for _, e := range e {
		row[0] = e.Path
		row[1] = e.Title
		row[2] = e.Published
		row[3] = e.Modified
		cw.Write(row)
	}
	cw.Flush()
	return cw.Error()
}
