package cmd

import (
	"context"
	"flag"
	"os"
	"sort"
	"strings"

	"github.com/google/subcommands"
	"go.felesatra.moe/felesatra/generator/internal/index"
	"go.felesatra.moe/felesatra/generator/internal/templates"
)

type IndexPage struct {
	templateDir string
}

func (*IndexPage) Name() string     { return "indexpage" }
func (*IndexPage) Synopsis() string { return "Render an index page." }
func (*IndexPage) Usage() string {
	return `indexpage [-templates DIR] INDEX:
  Render an index page.
`
}

func (c *IndexPage) SetFlags(f *flag.FlagSet) {
	f.StringVar(&c.templateDir, "templates", "templates",
		"Templates directory")
}

func (c *IndexPage) Execute(_ context.Context, f *flag.FlagSet, _ ...interface{}) subcommands.ExitStatus {
	if f.NArg() != 1 {
		eprintln("must provide one argument")
		return subcommands.ExitUsageError
	}
	p := f.Arg(0)
	t, err := templates.LoadIndexTemplate(c.templateDir)
	if err != nil {
		eprintln("error loading template:", err)
		return subcommands.ExitFailure
	}
	e, err := readIndex(p)
	if err != nil {
		eprintln("error loading index", err)
		return subcommands.ExitFailure
	}
	d := struct {
		Pages []index.Entry
	}{
		Pages: processIndexEntries(e),
	}
	if err := t.Execute(os.Stdout, d); err != nil {
		eprintln(err)
		return subcommands.ExitFailure
	}
	return subcommands.ExitSuccess
}

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

func processIndexEntries(e []index.Entry) []index.Entry {
	pages := make([]index.Entry, 0, len(e))
	blog := make([]index.Entry, 0, len(e))
	for _, e := range e {
		if isBlog(e) {
			blog = append(blog, e)
		} else if !is404(e) {
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

func is404(e index.Entry) bool {
	return e.Path == "404"
}
