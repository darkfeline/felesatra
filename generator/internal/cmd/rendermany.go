package cmd

import (
	"context"
	"flag"
	"fmt"
	"os"
	"path/filepath"

	"github.com/google/subcommands"

	"go.felesatra.moe/felesatra/generator/internal/template"
)

type RenderMany struct {
	templateDir string
}

func (*RenderMany) Name() string     { return "rendermany" }
func (*RenderMany) Synopsis() string { return "Render multiple pages." }
func (*RenderMany) Usage() string {
	return `rendermany [-templates DIR] SRC DST:
  Render multiple pages.
`
}

func (c *RenderMany) SetFlags(f *flag.FlagSet) {
	f.StringVar(&c.templateDir, "templates", "templates",
		"Templates directory")
}

func (c *RenderMany) Execute(_ context.Context, f *flag.FlagSet, _ ...interface{}) subcommands.ExitStatus {
	if f.NArg() != 2 {
		eprintln("must provide two arguments")
		return subcommands.ExitUsageError
	}
	srcdir := f.Arg(0)
	dstdir := f.Arg(1)
	t, err := template.Load(c.templateDir)
	if err != nil {
		eprintln("error loading templates:", err)
		return subcommands.ExitFailure
	}
	wf := func(path string, info os.FileInfo, err error) error {
		if err != nil {
			fmt.Fprintf(os.Stderr, "%s: error walking to %s: %s", os.Args[0], path, err)
			return err
		}
		if info.IsDir() {
			return nil
		}
		src := path
		rel := src[len(srcdir):]
		dst := filepath.Join(dstdir, rel)
		if err := os.MkdirAll(filepath.Dir(dst), 0777); err != nil {
			eprintln(err)
			return err
		}
		if err := template.RenderEnjaFile(t, src, dst); err != nil {
			eprintln(err)
			return err
		}
		return nil
	}
	if err := filepath.Walk(srcdir, wf); err != nil {
		return subcommands.ExitFailure
	}
	return subcommands.ExitSuccess
}
