package cmd

import (
	"context"
	"flag"
	"os"
	"path/filepath"

	"github.com/google/subcommands"

	"go.felesatra.moe/felesatra/generator/internal/template"
)

type Render struct {
	templateDir string
}

func (*Render) Name() string     { return "render" }
func (*Render) Synopsis() string { return "Render page." }
func (*Render) Usage() string {
	return `render [-templates DIR] SRC DST:
  Render page.
`
}

func (c *Render) SetFlags(f *flag.FlagSet) {
	f.StringVar(&c.templateDir, "templates", "templates",
		"Templates directory")
}

func (c *Render) Execute(_ context.Context, f *flag.FlagSet, _ ...interface{}) subcommands.ExitStatus {
	if f.NArg() != 2 {
		eprintln("must provide two arguments")
		return subcommands.ExitUsageError
	}
	src := f.Arg(0)
	dst := f.Arg(1)
	t, err := template.Load(c.templateDir)
	if err != nil {
		eprintln("error loading templates:", err)
		return subcommands.ExitFailure
	}
	if err := os.MkdirAll(filepath.Dir(dst), 0777); err != nil {
		eprintln(err)
		return subcommands.ExitFailure
	}
	if err := template.RenderEnjaFile(t, src, dst); err != nil {
		eprintln(err)
		return subcommands.ExitFailure
	}
	return subcommands.ExitSuccess
}
