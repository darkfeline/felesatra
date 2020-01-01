// Package index provides page indexing structures.
package index

import (
	"encoding/csv"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"

	"generator/internal/enja"
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
		if path[len(path)-1] == '~' {
			return nil
		}
		d, err := enja.LoadPath(path)
		if err != nil {
			return fmt.Errorf("index dir: %w", err)
		}
		en, err := docEntry(d)
		if err != nil {
			return fmt.Errorf("index dir: load %s: %w", path, err)
		}
		ext := filepath.Ext(path)
		en.Path = path[len(dir)+1 : len(path)-len(ext)]
		e = append(e, en)
		return nil
	}
	if err := filepath.Walk(dir, wf); err != nil {
		return nil, err
	}
	return e, nil
}

// docEntry creates an index entry for the document.
func docEntry(d *enja.Document) (e Entry, err error) {
	if v, ok := d.Header["title"].(string); ok {
		e.Title = v
	} else {
		return Entry{}, errors.New("missing title")
	}
	if v, ok := d.Header["published"].(string); ok {
		e.Published = v
	}
	if v, ok := d.Header["modified"].(string); ok {
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
