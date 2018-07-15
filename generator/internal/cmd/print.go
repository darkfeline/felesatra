package cmd

import (
	"fmt"
	"os"
)

func eprintf(format string, a ...interface{}) (int, error) {
	na := make([]interface{}, len(a)+1)
	na[0] = os.Args[0]
	for i, v := range a {
		na[i+1] = v
	}
	return fmt.Fprintf(os.Stderr, "%s: "+format, na...)
}

func eprintln(a ...interface{}) (int, error) {
	na := make([]interface{}, len(a)+1)
	na[0] = os.Args[0] + ":"
	for i, v := range a {
		na[i+1] = v
	}
	return fmt.Fprintln(os.Stderr, a...)
}
