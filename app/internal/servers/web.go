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
		withBasicAuth("yggdrasil", f),
	))
	m.Handle("/", chain(
		pageServer{fs: http.Dir("srv/www")},
		withCompress,
		withCacheControl("public,max-age=604800"), // 7d
	))
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
	h.serveFile(w, req, p)
}

func (h pageServer) serveFile(w http.ResponseWriter, req *http.Request, name string) {
	f, err := h.fs.Open(name)
	if err == nil {
		http.ServeContent(w, req, name, time.Time{}, f)
		return
	}
	if !os.IsNotExist(err) {
		log.Printf("Error: pageServer serve file %s: %s", name, err)
		http.Error(w, "Server Error", 500)
		return
	}

	name = name + ".html"
	f, err = h.fs.Open(name)
	if err == nil {
		http.ServeContent(w, req, name, time.Time{}, f)
		return
	}
	if !os.IsNotExist(err) {
		log.Printf("Error: pageServer serve file %s: %s", name, err)
		http.Error(w, "Server Error", 500)
		return
	}

	h.write404(w, req)
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
