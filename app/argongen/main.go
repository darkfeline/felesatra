package main

import (
	"crypto/rand"
	"fmt"
	"io"
	"os"

	"golang.org/x/crypto/argon2"
)

func main() {
	pw, err := io.ReadAll(os.Stdin)
	if err != nil {
		panic(err)
	}
	salt := make([]byte, 16)
	_, err = rand.Read(salt)
	if err != nil {
		panic(err)
	}
	out := argon2.IDKey(pw, salt, 1, 32*1024, 4, 32)
	fmt.Fprintf(os.Stdout, "\n%q\n%q\n", salt, out)
}
