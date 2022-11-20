package servers

import "net/http"

func NewGoproxy() http.Handler {
	return chain(
		http.FileServer(http.Dir("srv/goproxy")),
		withCacheControl("public,max-age=604800"), // 7d
	)
}
