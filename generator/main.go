package main

import (
	"fmt"
	"log"
	"os"
)

func main() {
	log.SetPrefix("gen: ")
	if err := innerMain(); err != nil {
		log.Fatal(err)
	}
}

type subcommand func() error

var subcommands = make(map[string]subcommand)

func innerMain() error {
	c, ok := subcommands[os.Args[1]]
	if !ok {
		return fmt.Errorf("unknown command %s", os.Args[1])
	}
	return c()
}
