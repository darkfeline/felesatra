package main

import (
	"testing"
)

func TestFindPackageExact(t *testing.T) {
	t.Parallel()
	p, ok := findPackage("/felesatra")
	if !ok {
		t.Errorf("Unexpected ok = %t", ok)
	}
	if p != pkgs["/felesatra"] {
		t.Errorf("Expected %+v, got %+v", pkgs["/felesatra"], p)
	}
}

func TestFindPackageSub(t *testing.T) {
	t.Parallel()
	p, ok := findPackage("/felesatra/foo")
	if !ok {
		t.Errorf("Unexpected ok = %t", ok)
	}
	if p != pkgs["/felesatra"] {
		t.Errorf("Expected %+v, got %+v", pkgs["/felesatra"], p)
	}
}

func TestFindPackageMissing(t *testing.T) {
	t.Parallel()
	_, ok := findPackage("/foobar")
	if ok {
		t.Errorf("Unexpected ok = %t", ok)
	}
}
