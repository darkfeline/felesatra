package main

import (
	"errors"
	"os"

	"kanade/internal/render"
	"kanade/internal/templates"
)

func init() {
	commands = append(commands, command{
		usage: "render SRC",
		f:     wrapCmd(renderCommand),
	})
}

func renderCommand(args []string) error {
	if len(args) != 1 {
		return errors.New("must provide one arguments")
	}
	src := args[0]
	t := templates.LoadPageTemplate()
	return render.RenderEnjaFile(t, os.Stdout, src)
}