package cmd

import (
	"context"
	"flag"
	"os"

	"github.com/google/subcommands"
	"go.felesatra.moe/felesatra/generator/internal/index"
	"go.felesatra.moe/felesatra/generator/internal/sitemap"
	"go.felesatra.moe/felesatra/generator/internal/templates"
)

type Sitemap struct {
	templateDir string
	prefix      string
}

func (*Sitemap) Name() string     { return "sitemap" }
func (*Sitemap) Synopsis() string { return "Render a sitemap." }
func (*Sitemap) Usage() string {
	return `sitemap [-templates DIR] [-prefix PREFIX] INDEX:
  Render a sitemap.
`
}

func (c *Sitemap) SetFlags(f *flag.FlagSet) {
	f.StringVar(&c.templateDir, "templates", "templates",
		"Templates directory")
	f.StringVar(&c.prefix, "prefix", "https://www.felesatra.moe/",
		"URL prefix")
}

func (c *Sitemap) Execute(_ context.Context, f *flag.FlagSet, _ ...interface{}) subcommands.ExitStatus {
	if f.NArg() != 1 {
		eprintln("must provide one argument")
		return subcommands.ExitUsageError
	}
	p := f.Arg(0)
	t, err := templates.LoadSitemapTemplate(c.templateDir)
	if err != nil {
		eprintln("error loading template:", err)
		return subcommands.ExitFailure
	}
	e, err := readIndex(p)
	if err != nil {
		eprintln("error loading index", err)
		return subcommands.ExitFailure
	}
	e = processIndexEntries(e)
	e2 := sitemapFromIndex(c.prefix, e)
	if err := t.Execute(os.Stdout, e2); err != nil {
		eprintln(err)
		return subcommands.ExitFailure
	}
	return subcommands.ExitSuccess
}

func sitemapFromIndex(prefix string, e []index.Entry) []sitemap.Entry {
	e2 := make([]sitemap.Entry, len(e))
	for i, v := range e {
		e2[i] = sitemap.Entry{
			Loc:     prefix + v.Path,
			LastMod: v.Modified,
		}
	}
	return e2
}
