package servers

import (
	"fmt"
	"net/http"
)

func withBasicAuth(h http.Handler, realm string, f CheckFunc) http.Handler {
	return authHandler{
		h:      h,
		header: fmt.Sprintf("Basic realm=\"%s\"", realm),
		f:      f,
	}
}

type CheckFunc func(user, pw string) bool

type authHandler struct {
	h      http.Handler
	header string
	f      CheckFunc
}

func (h authHandler) ServeHTTP(w http.ResponseWriter, req *http.Request) {
	u, pw, ok := req.BasicAuth()
	if !ok || !h.f(u, pw) {
		w.Header()["WWW-Authenticate"] = []string{h.header}
		w.WriteHeader(401)
		return
	}
	h.h.ServeHTTP(w, req)
}
