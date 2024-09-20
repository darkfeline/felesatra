package main

import (
	"io"
	"os"

	"golang.org/x/crypto/bcrypt"
)

func main() {
	b, err := io.ReadAll(os.Stdin)
	if err != nil {
		panic(err)
	}
	out, err := bcrypt.GenerateFromPassword(b, 12)
	if err != nil {
		panic(err)
	}
	os.Stdout.Write(out)
}
