package main

import (
	"errors"
	"os"

	"go.felesatra.moe/sitemap"

	"kanade/internal/index"
)

func init() {
	commands = append(commands, command{
		usage: "make-sitemap INDEX",
		f:     wrapCmd(sitemapCommand),
	})
}

const sitePrefix = "https://www.felesatra.moe/"

func sitemapCommand(args []string) error {
	if len(args) != 1 {
		return errors.New("must provide one argument")
	}
	path := args[0]
	e, err := readIndex(path)
	if err != nil {
		return err
	}
	e = sortIndexEntries(e)
	u := sitemap.URLSet{
		URLs: urlsFromIndex(sitePrefix, e),
	}
	return sitemap.Write(os.Stdout, &u)
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
