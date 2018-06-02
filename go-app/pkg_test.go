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
	exp := pkgMap["/felesatra"]
	if p != exp {
		t.Errorf("Expected %+v, got %+v", exp, p)
	}
}

func TestFindPackageSub(t *testing.T) {
	t.Parallel()
	p, ok := findPackage("/felesatra/foo")
	if !ok {
		t.Errorf("Unexpected ok = %t", ok)
	}
	exp := pkgMap["/felesatra"]
	if p != exp {
		t.Errorf("Expected %+v, got %+v", exp, p)
	}
}

func TestFindPackageMissing(t *testing.T) {
	t.Parallel()
	_, ok := findPackage("/foobar")
	if ok {
		t.Errorf("Unexpected ok = %t", ok)
	}
}
