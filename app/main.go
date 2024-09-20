package main

import (
	"crypto/subtle"
	"fmt"
	"log"
	"net/http"
	"os"
	"time"

	"app/internal/servers"

	"golang.org/x/crypto/argon2"
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
	m.Handle("files.felesatra.moe/", servers.NewFiles(filesBucket))
	m.Handle("openpgpkey.felesatra.moe/", servers.NewWKD())
	m.Handle("go.felesatra.moe/", servers.NewGo())
	m.Handle("goproxy.felesatra.moe/", servers.NewGoproxy())
	m.Handle("saphy.felesatra.moe/", servers.NewSaphy(checkAuth))
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

func checkAuth(u, pw string) (ok bool) {
	res := 1
	res &= subtle.ConstantTimeCompare([]byte(u), []byte(username))
	hash := argon2.IDKey([]byte(pw), []byte(argonSalt), 1, 32*1024, 4, 32)
	res &= subtle.ConstantTimeCompare(hash, []byte(passwordHash))
	return res == 1
}
