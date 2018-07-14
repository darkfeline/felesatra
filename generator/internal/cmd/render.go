package cmd

import (
	"context"
	"flag"
	"fmt"
	"os"

	"github.com/google/subcommands"
)

type Render struct {
}

func (*Render) Name() string     { return "render" }
func (*Render) Synopsis() string { return "Render page." }
func (*Render) Usage() string {
	return `render PAGE:
  Render page.
`
}

func (c *Render) SetFlags(f *flag.FlagSet) {
}

func (c *Render) Execute(_ context.Context, f *flag.FlagSet, _ ...interface{}) subcommands.ExitStatus {
	if f.NArg() < 1 {
		fmt.Fprintf(os.Stderr, c.Usage())
		return subcommands.ExitUsageError
	}
	return subcommands.ExitSuccess
}
