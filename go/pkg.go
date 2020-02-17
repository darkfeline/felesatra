package main

type pkgSpec struct {
	Path    string
	Method  Method
	URL     string
	RepoURL string
}

type Method string

const (
	Mod Method = "mod"
	Git Method = "git"
)

var modulePkgs = []pkgSpec{
	{Path: "/anidb", RepoURL: "https://github.com/darkfeline/anidb-go"},
	{Path: "/animanager", RepoURL: "https://github.com/darkfeline/animanager-go"},
	{Path: "/binpack", RepoURL: "https://github.com/darkfeline/binpack"},
	{Path: "/booru/dl", RepoURL: "https://github.com/darkfeline/booru-dl-go"},
	{Path: "/danbooru", RepoURL: "https://github.com/darkfeline/danbooru-go"},
	{Path: "/dlsite", RepoURL: "https://github.com/darkfeline/dlsite-go"},
	{Path: "/go2/errors", RepoURL: "https://github.com/darkfeline/go2-errors"},
	{Path: "/keeper", RepoURL: "https://github.com/darkfeline/keeper-go"},
	{Path: "/linelist", RepoURL: "https://github.com/darkfeline/go-linelist"},
	{Path: "/orbis", RepoURL: "https://github.com/darkfeline/orbis-go"},
	{Path: "/pwnck", RepoURL: "https://github.com/darkfeline/pwnck"},
	{Path: "/qualia", RepoURL: "https://github.com/darkfeline/qualia-go"},
	{Path: "/saucenao", RepoURL: "https://github.com/darkfeline/saucenao"},
	{Path: "/sitemap", RepoURL: "https://github.com/darkfeline/sitemap-go"},
	{Path: "/subcommands", RepoURL: "https://github.com/darkfeline/subcommands-go"},
	{Path: "/xdg", RepoURL: "https://github.com/darkfeline/xdg-go"},
}

var pkgMap = make(map[string]*pkgSpec)

func init() {
	for _, p := range modulePkgs {
		p := p
		p.Method = Mod
		p.URL = "https://goproxy.felesatra.moe"
		pkgMap[p.Path] = &p
	}
}
