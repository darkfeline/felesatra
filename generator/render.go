package main

import (
	"errors"
	"os"
	"path/filepath"

	"generator/internal/render"
	"generator/internal/templates"
)

func init() {
	subcommands["render"] = renderCommand
}

// Usage: render SRC DST
func renderCommand() error {
	args := os.Args[2:]
	if len(args) != 2 {
		return errors.New("must provide two arguments")
	}
	src := args[0]
	dst := args[1]
	t := templates.LoadPageTemplate()
	if err := os.MkdirAll(filepath.Dir(dst), 0777); err != nil {
		return err
	}
	if err := render.RenderEnjaFile(t, src, dst); err != nil {
		return err
	}
	return nil
}
