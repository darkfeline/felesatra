package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
	"time"

	"app/internal/servers"

	"golang.org/x/crypto/bcrypt"
)

func main() {
	port := os.Getenv("PORT")
	if port == "" {
		port = "8080"
		log.Printf("Defaulting to port %s", port)
	}
	log.Printf("Listening on port %s", port)
	s := makeServer(port)
	log.Fatal(s.ListenAndServe())
}

func makeServer(port string) *http.Server {
	m := http.NewServeMux()
	m.Handle("go.felesatra.moe/", servers.NewGo())
	m.Handle("goproxy.felesatra.moe/", servers.NewGoproxy())
	m.Handle("files.felesatra.moe/", servers.NewFiles(filesBucket))
	wh := servers.NewWeb(checkAuth)
	m.Handle("www.felesatra.moe/", wh)
	m.Handle("/", wh)
	return &http.Server{
		Addr:           fmt.Sprintf(":%s", port),
		Handler:        m,
		ReadTimeout:    10 * time.Second,
		WriteTimeout:   10 * time.Second,
		MaxHeaderBytes: 1 << 18, // 256 KiB
	}
}

// username and passwordHash defined in auth.go.
func checkAuth(u, pw string) (ok bool) {
	ok = true
	if u != username {
		ok = false
	}
	if err := bcrypt.CompareHashAndPassword([]byte(passwordHash), []byte(pw)); err != nil {
		ok = false
	}
	return ok
}
