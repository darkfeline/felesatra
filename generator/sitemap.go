package main

import (
	"os"

	"go.felesatra.moe/sitemap"
	"golang.org/x/xerrors"

	"go.felesatra.moe/felesatra/generator/internal/index"
)

func init() {
	subcommands["sitemap"] = sitemapCommand
}

const sitePrefix = "https://www.felesatra.moe/"

// Usage: sitemap INDEX
func sitemapCommand() error {
	args := os.Args[2:]
	if len(args) != 1 {
		return xerrors.New("must provide one argument")
	}
	path := args[0]
	e, err := readIndex(path)
	if err != nil {
		return err
	}
	e = processIndexEntries(e)
	u := sitemap.URLSet{
		URLs: urlsFromIndex(sitePrefix, e),
	}
	if err := sitemap.Write(os.Stdout, &u); err != nil {
		return err
	}
	return nil
}

func urlsFromIndex(prefix string, e []index.Entry) []sitemap.URL {
	urls := make([]sitemap.URL, len(e))
	for i, v := range e {
		urls[i] = sitemap.URL{
			Loc:     prefix + v.Path,
			LastMod: v.Modified,
		}
	}
	return urls
}