package servers

import "net/http"

func NewGoproxy() http.Handler {
	return withCacheControl(http.FileServer(http.Dir("srv/goproxy")),
		"public,max-age=604800") // 7d
}
