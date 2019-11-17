package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"strings"
)

func main() {
	port := os.Getenv("PORT")
	if port == "" {
		port = "8080"
		log.Printf("Defaulting to port %s", port)
	}
	log.Printf("Listening on port %s", port)
	http.HandleFunc("/", wrapError(handlePublic))
	http.Handle("/private/", http.StripPrefix("/private/", newPrivateFileServer()))
	log.Fatal(http.ListenAndServe(fmt.Sprintf(":%s", port), nil))
}

func handlePublic(w http.ResponseWriter, r *http.Request) error {
	p := r.URL.Path
	if p == "/" {
		p = "index"
	}
	if strings.HasSuffix(p, "/") {
		h := w.Header()
		h["Location"] = []string{strings.TrimRight(p, "/")}
		w.WriteHeader(301)
		return nil
	}
	d, err := readPage(p)
	if err != nil {
		if os.IsNotExist(err) {
			return write404(w)
		}
		return fmt.Errorf("Error reading page: %s", err)
	}
	w.Write(d)
	return nil
}

// write404 writes a 404 response.
func write404(w http.ResponseWriter) error {
	d, err := readPage("404")
	if err != nil {
		return fmt.Errorf("Error reading 404 page: %s", err)
	}
	w.WriteHeader(404)
	w.Write(d)
	return nil
}

type handlerFunc func(http.ResponseWriter, *http.Request)
type handlerFuncE func(http.ResponseWriter, *http.Request) error

func wrapError(f handlerFuncE) handlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		if err := f(w, r); err != nil {
			log.Printf("Handler error: %s", err)
			http.Error(w, "Server Error", 500)
		}
	}
}

func readPage(p string) ([]byte, error) {
	return ioutil.ReadFile(fmt.Sprintf("pages/%s.html", p))
}

func newPrivateFileServer() authHandler {
	return authHandler{
		h: http.FileServer(http.Dir("private")),
	}
}

type authHandler struct {
	h http.Handler
}

func (s authHandler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	u, pw, ok := r.BasicAuth()
	if !ok {
		writeBasicAuth(w)
		return
	}
	if !checkAuth(u, pw) {
		writeBasicAuth(w)
		return
	}
	s.h.ServeHTTP(w, r)
	return
}

func writeBasicAuth(w http.ResponseWriter) {
	h := w.Header()
	h["WWW-Authenticate"] = []string{"Basic realm=\"yggdrasil\""}
	w.WriteHeader(401)
}
