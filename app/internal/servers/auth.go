package servers

import (
	"fmt"
	"net/http"
)

func withBasicAuth(realm string, f CheckFunc) middleware {
	header := fmt.Sprintf("Basic realm=\"%s\"", realm)
	return func(h http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
			u, pw, ok := req.BasicAuth()
			if !ok || !f(u, pw) {
				w.Header()["WWW-Authenticate"] = []string{header}
				w.WriteHeader(401)
				return
			}
			h.ServeHTTP(w, req)
		})
	}
}

type CheckFunc func(user, pw string) bool
