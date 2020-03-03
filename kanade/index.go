package main

import (
	"errors"
	"os"

	"kanade/internal/index"
)

func init() {
	commands = append(commands, command{
		usage: "index-pages DIR",
		f:     wrapCmd(indexCommand),
	})
}

func indexCommand(args []string) error {
	if len(args) != 1 {
		return errors.New("must provide one arguments")
	}
	dir := args[0]
	e, err := index.IndexDir(dir)
	if err != nil {
		return err
	}
	return index.WriteAll(os.Stdout, e)
}
