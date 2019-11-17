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
	http.HandleFunc("/private/", wrapError(handlePrivate))
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

func handlePrivate(w http.ResponseWriter, r *http.Request) error {
	u, pw, ok := r.BasicAuth()
	if !ok {
		return writeBasicAuth(w)
	}
	if !checkAuth(u, pw) {
		return writeBasicAuth(w)
	}
	d, err := readPrivatePage(r.URL.Path)
	if err != nil {
		http.Error(w, "Not Found", 404)
		return nil
	}
	w.Write(d)
	return nil
}

func writeBasicAuth(w http.ResponseWriter) error {
	h := w.Header()
	h["WWW-Authenticate"] = []string{"Basic realm=\"yggdrasil\""}
	w.WriteHeader(401)
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

func readPrivatePage(p string) ([]byte, error) {
	return ioutil.ReadFile(fmt.Sprintf("private/%s", p))
}
