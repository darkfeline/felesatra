package main

import (
	"path"
	"strings"
)

var pkgs = []Package{}

var modulePkgs = []string{
	"/anidb",
	"/animanager",
	"/binpack",
	"/booru/dl",
	"/danbooru",
	"/dlsite",
	"/go2/errors",
	"/keeper",
	"/linelist",
	"/orbis",
	"/pwnck",
	"/qualia",
	"/saucenao",
	"/sitemap",
	"/subcommands",
	"/xdg",
}

var pkgMap = make(map[string]*Package)

type Package struct {
	Path   string
	Method Method
	URL    string
}

type Method string

const (
	Mod Method = "mod"
	Git Method = "git"
)

// findPackage finds the longest matching package prefix for the given path.
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
	for _, path := range modulePkgs {
		p := Package{
			Path:   path,
			Method: Mod,
			URL:    "https://goproxy.felesatra.moe",
		}
		pkgMap[p.Path] = &p
	}
}
