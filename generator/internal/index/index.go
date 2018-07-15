// Package index provides page indexing structures.
package index

import (
	"encoding/csv"
	"io"
	"os"
	"path/filepath"

	"github.com/pkg/errors"
	"go.felesatra.moe/felesatra/generator/internal/enja"
	"go.felesatra.moe/felesatra/generator/internal/template"
)

type Entry struct {
	Path      string
	Title     string
	Published string
	Modified  string
}

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
		d, err := template.ReadEnjaFile(path)
		if err != nil {
			return errors.Wrap(err, "read enja file")
		}
		en, err := docEntry(d)
		if err != nil {
			return err
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

func docEntry(d *enja.Document) (e Entry, err error) {
	defer func() {
		if r := recover(); r != nil {
			switch r := r.(type) {
			case error:
				err = r
			default:
				err = errors.Errorf("panic in docEntry: %v", r)
			}
		}
	}()
	return Entry{
		Title:     d.Header["title"].(string),
		Published: d.Header["published"].(string),
		Modified:  d.Header["modified"].(string),
	}, nil
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
