package servers

import "net/http"

func NewFiles() http.Handler {
	return withCacheControl(http.FileServer(http.Dir("srv/files")),
		"public,max-age=604800") // 7d
}
