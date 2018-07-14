package cmd

import (
	"context"
	"flag"
	"fmt"
	"os"
	"path/filepath"

	"github.com/google/subcommands"
	"go.felesatra.moe/felesatra/generator/internal/enja"
)

type Render struct {
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
		fmt.Fprintf(os.Stderr, "%s: must provide two arguments\n", os.Args[0])
		return subcommands.ExitUsageError
	}
	src := f.Arg(0)
	dst := f.Arg(1)
	if err := os.MkdirAll(filepath.Dir(dst), 0777); err != nil {
		fmt.Fprintf(os.Stderr, "%s: %s", os.Args[0], err)
		return subcommands.ExitFailure
	}
	if err := enja.Render(src, dst); err != nil {
		fmt.Fprintf(os.Stderr, "%s: %s", os.Args[0], err)
		return subcommands.ExitFailure
	}
	return subcommands.ExitSuccess
}
