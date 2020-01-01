package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
	"time"
)

func main() {
	port := os.Getenv("PORT")
	if port == "" {
		port = "8080"
		log.Printf("Defaulting to port %s", port)
	}
	log.Printf("Listening on port %s", port)
	m := http.NewServeMux()
	m.Handle("/", newPublicHandler())
	m.Handle("/private/", http.StripPrefix("/private/", newPrivateFileServer()))
	s := &http.Server{
		Addr:           fmt.Sprintf(":%s", port),
		Handler:        m,
		ReadTimeout:    10 * time.Second,
		WriteTimeout:   10 * time.Second,
		MaxHeaderBytes: 1 << 18, // 256 KiB
	}
	log.Fatal(s.ListenAndServe())
}
