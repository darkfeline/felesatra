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
		return errors.New("must provide one argument")
	}
	src := args[0]
	return render.RenderEnjaFile(templates.BaseTemplate, os.Stdout, src)
}
