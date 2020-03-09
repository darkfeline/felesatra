package servers

import (
	"fmt"
	"net/http"
)

func NewFiles(bucket string) http.Handler {
	prefix := fmt.Sprintf("https://storage.cloud.google.com/%s", bucket)
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header()["Cache-Control"] = []string{"public,max-age=604800"} // 7d
		http.Redirect(w, r, prefix+r.URL.Path, http.StatusTemporaryRedirect)
	})
}
