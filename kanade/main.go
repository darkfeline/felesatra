package main

import (
	"fmt"
	"log"
	"os"
	"strings"
)

func main() {
	log.SetPrefix("kanade: ")
	if len(os.Args) < 1 {
		printCommands()
		os.Exit(0)
	}
	c, ok := lookupCommand(os.Args[1])
	if !ok {
		log.Printf("unknown command %s", os.Args[1])
		os.Exit(1)
	}
	c.f()
}

func printCommands() {
	for _, c := range commands {
		fmt.Println(c.usage)
	}
}

func lookupCommand(name string) (c command, ok bool) {
	for _, c := range commands {
		if c.name() == name {
			return c, true
		}
	}
	return command{}, false
}

var commands []command

type command struct {
	usage string
	f     func()
}

func (c command) name() string {
	return strings.SplitN(c.usage, " ", 2)[0]
}

// wrapCmd wraps a command function that returns an error.
func wrapCmd(f func(args []string) error) func() {
	return func() {
		if err := f(os.Args[2:]); err != nil {
			log.Fatal(err)
		}
	}
}
