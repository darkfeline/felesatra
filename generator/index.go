package main

import (
	"os"

	"go.felesatra.moe/felesatra/generator/internal/index"
	"golang.org/x/xerrors"
)

func init() {
	subcommands["index"] = indexCommand
}

// Usage: index OUTFILE DIR
func indexCommand() error {
	args := os.Args[2:]
	if len(args) != 2 {
		return xerrors.New("must provide two arguments")
	}
	out := args[0]
	dir := args[1]
	e, err := index.IndexDir(dir)
	if err != nil {
		return err
	}
	fi, err := os.Create(out)
	if err != nil {
		return err
	}
	defer fi.Close()
	if err := index.WriteAll(fi, e); err != nil {
		return err
	}
	if err := fi.Close(); err != nil {
		return err
	}
	return nil
}
