package servers

import (
	"io"
	"log"
	"net/http"
	"os"
	"strings"
	"time"
)

func NewWeb(f CheckFunc) http.Handler {
	m := http.NewServeMux()
	m.Handle("/private/", withBasicAuth(
		http.FileServer(http.Dir("srv/www")),
		"yggdrasil", f))
	m.Handle("/", withCacheControl(newPublicWeb(),
		"public,max-age=604800")) // 7d
	return m
}

func newPublicWeb() http.Handler {
	fs := http.FileServer(http.Dir("srv/www"))
	m := http.NewServeMux()
	m.Handle("/css/", fs)
	m.Handle("/img/", fs)
	m.Handle("/", pageServer{fs: http.Dir("srv/www")})
	return m
}

type pageServer struct {
	fs http.FileSystem
}

func (h pageServer) ServeHTTP(w http.ResponseWriter, req *http.Request) {
	p := req.URL.Path
	switch p {
	case "/atom.xml", "/sitemap.xml":
		fallthrough
	case "/robots.txt", "/keybase.txt":
		h.serveFile(w, req, p)
		return
	}
	if p == "/" {
		p = "/index"
	}
	if strings.HasSuffix(p, "/") {
		writeRedirect(w, strings.TrimRight(p, "/"))
		return
	}
	h.serveFile(w, req, p+".html")
}

func (h pageServer) serveFile(w http.ResponseWriter, req *http.Request, name string) {
	f, err := h.fs.Open(name)
	if err != nil {
		if os.IsNotExist(err) {
			h.write404(w)
			return
		}
		log.Printf("Error: pageServer serve file %s: %s", name, err)
		http.Error(w, "Server Error", 500)
		return
	}
	http.ServeContent(w, req, name, time.Time{}, f)
}

func (h pageServer) write404(w http.ResponseWriter) {
	w.WriteHeader(404)
	f, err := h.fs.Open("404.html")
	if err != nil {
		log.Printf("Error reading 404 page: %s", err)
		w.Write([]byte("Page not found"))
		return
	}
	_, _ = io.Copy(w, f)
}

func writeRedirect(w http.ResponseWriter, p string) {
	w.Header()["Location"] = []string{p}
	w.WriteHeader(301)
}
