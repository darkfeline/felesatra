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
	http.HandleFunc("/", handle)
	log.Fatal(http.ListenAndServe(fmt.Sprintf(":%s", port), nil))
}

func handle(w http.ResponseWriter, r *http.Request) {
	p := r.URL.Path
	if p == "/" {
		p = "index"
	}
	if strings.HasSuffix(p, "/") {
		h := w.Header()
		h["Location"] = []string{strings.TrimRight(p, "/")}
		w.WriteHeader(301)
		return
	}
	d, err := readPage(p)
	if err != nil {
		if os.IsNotExist(err) {
			write404(w)
			return
		}
		log.Printf("Error reading page: %s", err)
		serverError(w)
		return
	}
	_, err = w.Write(d)
	if err != nil {
		log.Printf("Error writing response: %s", err)
		serverError(w)
		return
	}
}

// write404 writes a 404 response.
func write404(w http.ResponseWriter) {
	d, err := readPage("404")
	if err != nil {
		log.Printf("Error reading 404 page: %s", err)
		serverError(w)
		return
	}
	w.WriteHeader(404)
	_, err = w.Write(d)
	if err != nil {
		log.Printf("Error writing 404 response: %s", err)
		serverError(w)
		return
	}
}

// serverError writes a generic 500 response.
func serverError(w http.ResponseWriter) {
	w.WriteHeader(500)
	_, err := w.Write([]byte("Error"))
	if err != nil {
		log.Printf("Error sending server error: %s", err)
	}
}

func readPage(p string) ([]byte, error) {
	return ioutil.ReadFile(page(p))
}

func page(p string) string {
	return fmt.Sprintf("pages/%s.html", p)
}
