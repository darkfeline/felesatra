package main

import (
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"strings"
	"time"
)

func newPublicHandler() http.Handler {
	return publicHandler{
		fs: http.Dir("pages"),
	}
}

type publicHandler struct {
	fs http.FileSystem
}

func (h publicHandler) ServeHTTP(w http.ResponseWriter, req *http.Request) {
	p := req.URL.Path
	if p == "/" {
		p = "index"
	}
	if strings.HasSuffix(p, "/") {
		writeRedirect(w, strings.TrimRight(p, "/"))
		return
	}
	p = p + ".html"
	f, err := h.fs.Open(p)
	if err != nil {
		if os.IsNotExist(err) {
			write404(w)
			return
		}
		log.Printf("Public handler error: %s", err)
		http.Error(w, "Server Error", 500)
		return
	}
	w = &cacheWriter{
		ResponseWriter: w,
		// Cache for one week.
		cacheControl: []string{"public,max-age=604800"},
	}
	http.ServeContent(w, req, "foo.html", time.Time{}, f)
	return
}

func writeRedirect(w http.ResponseWriter, p string) {
	h := w.Header()
	h["Location"] = []string{p}
	w.WriteHeader(301)
}

// write404 writes a 404 response.
func write404(w http.ResponseWriter) {
	d, err := ioutil.ReadFile("pages/404.html")
	if err != nil {
		log.Printf("Error reading 404 page: %s", err)
		d = []byte("Page not found")
	}
	w.WriteHeader(404)
	w.Write(d)
}

func newPrivateFileServer() http.Handler {
	return privateHandler{
		fs: http.FileServer(http.Dir("private")),
	}
}

type privateHandler struct {
	fs http.Handler
}

func (h privateHandler) ServeHTTP(w http.ResponseWriter, req *http.Request) {
	u, pw, ok := req.BasicAuth()
	if !ok {
		writeBasicAuth(w)
		return
	}
	// checkAuth function needs to be defined separately.
	if !checkAuth(u, pw) {
		writeBasicAuth(w)
		return
	}
	w = &cacheWriter{
		ResponseWriter: w,
		// Cache for one day.
		cacheControl: []string{"private,max-age=86400"},
	}
	h.fs.ServeHTTP(w, req)
}

func writeBasicAuth(w http.ResponseWriter) {
	h := w.Header()
	h["WWW-Authenticate"] = []string{"Basic realm=\"yggdrasil\""}
	w.WriteHeader(401)
}

// cacheWriter implements a http.ResponseWriter that adds a
// Cache-Control header when writing 200 OK responses.
type cacheWriter struct {
	http.ResponseWriter
	cacheControl  []string
	headerWritten bool
}

func (w *cacheWriter) Write(d []byte) (int, error) {
	if !w.headerWritten {
		w.WriteHeader(http.StatusOK)
	}
	return w.ResponseWriter.Write(d)
}

func (w *cacheWriter) WriteHeader(c int) {
	w.headerWritten = true
	if c == http.StatusOK {
		w.ResponseWriter.Header()["Cache-Control"] = w.cacheControl
	}
	w.ResponseWriter.WriteHeader(c)
}
