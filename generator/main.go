package main

import (
	"context"
	"flag"
	"os"

	"github.com/google/subcommands"
	"go.felesatra.moe/felesatra/generator/internal/cmd"
)

func main() {
	subcommands.Register(subcommands.HelpCommand(), "")
	subcommands.Register(subcommands.FlagsCommand(), "")
	subcommands.Register(subcommands.CommandsCommand(), "")
	subcommands.Register(&cmd.Index{}, "")
	subcommands.Register(&cmd.IndexPage{}, "")
	subcommands.Register(&cmd.Render{}, "")
	subcommands.Register(&cmd.RenderMany{}, "")
	subcommands.Register(&cmd.Sitemap{}, "")

	flag.Parse()
	ctx := context.Background()
	os.Exit(int(subcommands.Execute(ctx)))
}
