package cmd

import (
	"context"
	"flag"
	"os"
	"path/filepath"

	"github.com/google/subcommands"

	"go.felesatra.moe/felesatra/generator/internal/render"
	"go.felesatra.moe/felesatra/generator/internal/templates"
)

type Render struct {
	templateDir string
}

func (*Render) Name() string     { return "render" }
func (*Render) Synopsis() string { return "Render page." }
func (*Render) Usage() string {
	return `render SRC DST:
  Render page.
`
}

func (c *Render) SetFlags(f *flag.FlagSet) {
}

func (c *Render) Execute(_ context.Context, f *flag.FlagSet, _ ...interface{}) subcommands.ExitStatus {
	if f.NArg() != 2 {
		eprintln("must provide two arguments")
		return subcommands.ExitUsageError
	}
	src := f.Arg(0)
	dst := f.Arg(1)
	t := templates.LoadPageTemplate()
	if err := os.MkdirAll(filepath.Dir(dst), 0777); err != nil {
		eprintln(err)
		return subcommands.ExitFailure
	}
	if err := render.RenderEnjaFile(t, src, dst); err != nil {
		eprintln(err)
		return subcommands.ExitFailure
	}
	return subcommands.ExitSuccess
}
