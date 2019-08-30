package main

import (
	"testing"
)

func TestFindPackage(t *testing.T) {
	t.Run("exact match", func(t *testing.T) {
		t.Parallel()
		p, ok := findPackage("/animanager")
		if !ok {
			t.Fatalf("Package not found")
		}
		if p.Path != "/animanager" {
			t.Errorf("Got unexpected package %#v", p)
		}
	})
	t.Run("subpath match", func(t *testing.T) {
		t.Parallel()
		p, ok := findPackage("/animanager/foo")
		if !ok {
			t.Fatalf("Package not found")
		}
		if p.Path != "/animanager" {
			t.Errorf("Got unexpected package %#v", p)
		}
	})
	t.Run("no match", func(t *testing.T) {
		t.Parallel()
		p, ok := findPackage("/foobar")
		if ok {
			t.Errorf("Got unexpected package %#v", p)
		}
	})
}
