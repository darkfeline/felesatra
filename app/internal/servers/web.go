package servers

import (
	"io"
	"log"
	"net/http"
	"os"
	"time"
)

func NewWeb(f CheckFunc) http.Handler {
	m := http.NewServeMux()
	m.Handle("/private/", chain(
		http.FileServer(http.Dir("srv/www")),
		withCompress,
		withBasicAuth("yggdrasil", f)))
	m.Handle("/", chain(
		newPublicWeb(),
		withCacheControl("public,max-age=604800"), // 7d
	))
	return m
}

func newPublicWeb() http.Handler {
	fs := http.FileServer(http.Dir("srv/www"))
	m := http.NewServeMux()
	m.Handle("/css/", fs)
	m.Handle("/img/", fs)
	m.Handle("/js/", fs)
	m.Handle("/atom.xml", fs)
	m.Handle("/sitemap.xml", fs)
	m.Handle("/identity.txt", fs)
	m.Handle("/robots.txt", fs)
	m.Handle("/", pageServer{fs: http.Dir("srv/www")})
	return m
}

type pageServer struct {
	fs http.FileSystem
}

func (h pageServer) ServeHTTP(w http.ResponseWriter, req *http.Request) {
	p := req.URL.Path
	switch {
	case p == "/":
		p = "/index"
	case p[len(p)-1] == '/':
		http.Redirect(w, req, p[:len(p)-1], http.StatusPermanentRedirect)
		return
	}
	h.serveFile(w, req, p+".html")
}

func (h pageServer) serveFile(w http.ResponseWriter, req *http.Request, name string) {
	f, err := h.fs.Open(name)
	if err != nil {
		if os.IsNotExist(err) {
			h.write404(w, req)
			return
		}
		log.Printf("Error: pageServer serve file %s: %s", name, err)
		http.Error(w, "Server Error", 500)
		return
	}
	http.ServeContent(w, req, name, time.Time{}, f)
}

func (h pageServer) write404(w http.ResponseWriter, r *http.Request) {
	f, err := h.fs.Open("404.html")
	if err != nil {
		log.Printf("Error reading 404 page: %s", err)
		http.NotFound(w, r)
		return
	}
	w.WriteHeader(404)
	_, _ = io.Copy(w, f)
}
