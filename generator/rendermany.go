package main

import (
	"os"
	"path/filepath"

	"golang.org/x/xerrors"

	"go.felesatra.moe/felesatra/generator/internal/render"
	"go.felesatra.moe/felesatra/generator/internal/templates"
)

func init() {
	subcommands["rendermany"] = rendermanyCommand
}

// Usage: rendermany SRC DST
func rendermanyCommand() error {
	args := os.Args[2:]
	if len(args) != 2 {
		return xerrors.New("must provide two arguments")
	}
	srcdir := filepath.Clean(args[0])
	dstdir := args[1]
	t := templates.LoadPageTemplate()
	wf := func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if info.IsDir() {
			return nil
		}
		src := path
		rel := src[len(srcdir)+1:]
		dst := filepath.Join(dstdir, rel)
		if err := os.MkdirAll(filepath.Dir(dst), 0777); err != nil {
			return err
		}
		if err := render.RenderEnjaFile(t, src, dst); err != nil {
			return err
		}
		return nil
	}
	if err := filepath.Walk(srcdir, wf); err != nil {
		return err
	}
	return nil
}