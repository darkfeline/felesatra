package cmd

import (
	"context"
	"flag"
	"os"

	"github.com/google/subcommands"
	"go.felesatra.moe/sitemap"

	"go.felesatra.moe/felesatra/generator/internal/index"
)

type Sitemap struct {
	templateDir string
	prefix      string
}

func (*Sitemap) Name() string     { return "sitemap" }
func (*Sitemap) Synopsis() string { return "Render a sitemap." }
func (*Sitemap) Usage() string {
	return `sitemap [-prefix PREFIX] INDEX:
  Render a sitemap.
`
}

func (c *Sitemap) SetFlags(f *flag.FlagSet) {
	f.StringVar(&c.prefix, "prefix", "https://www.felesatra.moe/",
		"URL prefix")
}

func (c *Sitemap) Execute(_ context.Context, f *flag.FlagSet, _ ...interface{}) subcommands.ExitStatus {
	if f.NArg() != 1 {
		eprintln("must provide one argument")
		return subcommands.ExitUsageError
	}
	path := f.Arg(0)
	e, err := readIndex(path)
	if err != nil {
		eprintln("error loading index", err)
		return subcommands.ExitFailure
	}
	e = processIndexEntries(e)
	u := sitemap.URLSet{
		URLs: urlsFromIndex(c.prefix, e),
	}
	if err := sitemap.Write(os.Stdout, &u); err != nil {
		eprintln(err)
		return subcommands.ExitFailure
	}
	return subcommands.ExitSuccess
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
