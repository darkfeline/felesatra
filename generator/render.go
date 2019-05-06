package main

import (
	"os"
	"path/filepath"

	"go.felesatra.moe/felesatra/generator/internal/render"
	"go.felesatra.moe/felesatra/generator/internal/templates"
	"golang.org/x/xerrors"
)

func init() {
	subcommands["render"] = renderCommand
}

// Usage: render SRC DST
func renderCommand() error {
	args := os.Args[2:]
	if len(args) != 2 {
		return xerrors.New("must provide two arguments")
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
