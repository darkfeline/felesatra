package main

import (
	"path"
	"strings"
)

var pkgs = []Package{
	{"/anidb", Git, "https://github.com/darkfeline/anidb-go"},
	{"/animanager", Git, "https://github.com/darkfeline/animanager-go"},
	{"/booru/dl", Git, "https://github.com/darkfeline/booru-dl-go"},
	{"/dlsite", Git, "https://github.com/darkfeline/dlsite-go"},
	{"/felesatra", Git, "https://github.com/darkfeline/felesatra"},
	{"/go2/errors", Git, "https://github.com/darkfeline/go2-errors"},
	{"/orbis", Git, "https://github.com/darkfeline/orbis-go"},
	{"/pwnck", Git, "https://github.com/darkfeline/pwnck"},
	{"/qualia", Git, "https://github.com/darkfeline/qualia-go"},
	{"/subcommands", Git, "https://github.com/darkfeline/subcommands-go"},
	{"/xdg", Git, "https://github.com/darkfeline/xdg-go"},
}

var pkgMap = make(map[string]*Package)

type Package struct {
	Path   string
	Method Method
	URL    string
}

type Method string

const (
	Git Method = "git"
)

func findPackage(pp string) (p *Package, ok bool) {
	for ; pp != "/"; pp, _ = path.Split(pp) {
		pp = strings.TrimRight(pp, "/")
		p, ok := pkgMap[pp]
		if ok {
			return p, true
		}
	}
	return nil, false
}

func init() {
	for i, p := range pkgs {
		pkgMap[p.Path] = &pkgs[i]
	}
}