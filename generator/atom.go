package main

import (
	"encoding/xml"
	"fmt"
	"os"
	"time"

	"go.felesatra.moe/felesatra/generator/internal/index"
	"golang.org/x/tools/blog/atom"
	"golang.org/x/xerrors"
)

func init() {
	subcommands["atom"] = atomCommand
}

// Usage: atom INDEX
func atomCommand() error {
	args := os.Args[2:]
	if len(args) != 1 {
		return xerrors.New("must provide one argument")
	}
	path := args[0]
	e, err := readIndex(path)
	if err != nil {
		return err
	}
	e = sortIndexEntries(e)
	f := atom.Feed{
		ID:    sitePrefix,
		Title: "Feles Atra",
		Link: []atom.Link{{
			Href: sitePrefix,
			Rel:  "self",
			Type: "text/html",
		}},
		Author: &atom.Person{
			Name: "Allen Li",
		},
		Updated: atom.Time(time.Now()),
	}
	for _, e := range e {
		ae, err := convertEntryToAtom(e)
		if err != nil {
			return err
		}
		f.Entry = append(f.Entry, ae)
	}
	fmt.Print(xml.Header)
	enc := xml.NewEncoder(os.Stdout)
	if err := enc.Encode(f); err != nil {
		return err
	}
	return nil
}

func convertEntryToAtom(e index.Entry) (*atom.Entry, error) {
	pt, err := convertTimeToAtom(e.Published)
	if err != nil {
		return nil, xerrors.Errorf("convert entry %#v to atom: %v", e, err)
	}
	mt, err := convertTimeToAtom(e.Modified)
	if err != nil {
		return nil, xerrors.Errorf("convert entry %#v to atom: %v", e, err)
	}
	return &atom.Entry{
		ID:    sitePrefix + e.Path,
		Title: e.Title,
		Link: []atom.Link{{
			Href: sitePrefix + e.Path,
			Rel:  "self",
			Type: "text/html",
		}},
		Published: pt,
		Updated:   mt,
	}, nil
}

func convertTimeToAtom(ts string) (atom.TimeStr, error) {
	t, err := time.Parse("2006-01-02", ts)
	if err != nil {
		return "", xerrors.Errorf("convert time %v to atom: %v", ts, err)
	}
	return atom.Time(t), nil
}
