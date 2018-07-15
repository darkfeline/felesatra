package cmd

import (
	"context"
	"flag"
	"os"

	"github.com/google/subcommands"
	"go.felesatra.moe/felesatra/generator/internal/index"
)

type Index struct {
}

func (*Index) Name() string     { return "index" }
func (*Index) Synopsis() string { return "Index pages." }
func (*Index) Usage() string {
	return `index OUTFILE DIR:
  Index pages.
`
}

func (c *Index) SetFlags(f *flag.FlagSet) {
}

func (c *Index) Execute(_ context.Context, f *flag.FlagSet, _ ...interface{}) subcommands.ExitStatus {
	if f.NArg() != 2 {
		eprintln("must provide two arguments")
		return subcommands.ExitUsageError
	}
	out := f.Arg(0)
	dir := f.Arg(1)
	e, err := index.IndexDir(dir)
	if err != nil {
		eprintln("error indexing files:", err)
		return subcommands.ExitFailure
	}
	fi, err := os.Create(out)
	if err != nil {
		eprintln(err)
		return subcommands.ExitFailure
	}
	defer fi.Close()
	if err := index.WriteAll(fi, e); err != nil {
		eprintln(err)
		return subcommands.ExitFailure
	}
	if err := fi.Close(); err != nil {
		eprintln(err)
		return subcommands.ExitFailure
	}
	return subcommands.ExitSuccess
}
