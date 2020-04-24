package main

import (
	"errors"
	"os"
	"sort"
	"strings"

	"kanade/internal/index"
	"kanade/internal/templates"
)

func init() {
	commands = append(commands, command{
		usage: "make-index-page INDEX",
		f:     wrapCmd(indexpageCommand),
	})
}

func indexpageCommand(args []string) error {
	if len(args) != 1 {
		return errors.New("must provide one argument")
	}
	p := args[0]
	e, err := readIndex(p)
	if err != nil {
		return err
	}
	d := templates.IndexData{
		Pages: sortIndexEntries(e),
	}
	return templates.IndexTemplate.Execute(os.Stdout, d)
}

// readIndex reads in the index file.
func readIndex(path string) ([]index.Entry, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	e, err := index.ReadAll(f)
	if err != nil {
		return nil, err
	}
	return e, nil
}

// sortIndexEntries sorts the index entries.
// Pages come first, followed by blog posts in reverse chronological
// order.
func sortIndexEntries(e []index.Entry) []index.Entry {
	pages := make([]index.Entry, 0, len(e))
	blog := make([]index.Entry, 0, len(e))
	for _, e := range e {
		if isBlog(e) {
			blog = append(blog, e)
		} else {
			pages = append(pages, e)
		}
	}
	sort.Slice(pages, func(i, j int) bool { return pages[i].Path < pages[j].Path })
	sort.Slice(blog, func(i, j int) bool { return blog[i].Path >= blog[j].Path })
	n := make([]index.Entry, len(pages)+len(blog))
	for i, v := range pages {
		n[i] = v
	}
	l := len(pages)
	for i, v := range blog {
		n[l+i] = v
	}
	return n
}

func isBlog(e index.Entry) bool {
	return strings.HasPrefix(e.Path, "blog/")
}
