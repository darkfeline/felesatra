package servers

import (
	"app/internal/gopkg"
	"net/http"
	"path"
	"strings"
)

func NewGoproxy() http.Handler {
	return withCacheControl(http.FileServer(http.Dir("srv/goproxy")),
		"public,max-age=604800") // 7d
}

func NewGo() http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		ps, ok := findPackage(r.URL.Path)
		if !ok {
			w.WriteHeader(404)
			w.Write([]byte("Package not found"))
			return
		}
		w.Header()["Cache-Control"] = []string{"public,max-age=604800"} // 7d
		gopkg.Template.Execute(w, ps)
	})
}

// findPackage finds the longest matching package prefix for the given path.
func findPackage(p string) (ps gopkg.Spec, ok bool) {
	for ; p != "/"; p, _ = path.Split(p) {
		p = strings.TrimRight(p, "/")
		ps, ok := pkgMap[p]
		if ok {
			return ps, true
		}
	}
	return ps, false
}

var modulePkgs = []gopkg.Spec{
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

var pkgMap map[string]gopkg.Spec

func init() {
	pkgMap = make(map[string]gopkg.Spec, len(modulePkgs))
	for _, p := range modulePkgs {
		p.Method = gopkg.Mod
		p.URL = "https://goproxy.felesatra.moe"
		pkgMap[p.Path] = p
	}
}
