package main

import (
	"errors"
	"html/template"
	"os"

	"kanade/internal/enja"
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
	ed, err := enja.LoadPath(src)
	if err != nil {
		return err
	}
	d := templates.BaseData{
		Title:     ed.Header["title"],
		Published: ed.Header["published"],
		Modified:  ed.Header["modified"],
		Body:      template.HTML(ed.Body),
	}
	return templates.BaseTemplate.Execute(os.Stdout, d)
}
