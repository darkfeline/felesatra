package servers

import (
	"fmt"
	"net/http"
)

func NewFiles(bucket string) http.Handler {
	prefix := fmt.Sprintf("https://storage.cloud.google.com/%s", bucket)
	return chain(
		http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			http.Redirect(w, r, prefix+r.URL.Path, http.StatusTemporaryRedirect)
		}),
		withCacheControl("public,max-age=604800"), // 7d
	)
}
