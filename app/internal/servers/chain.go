package servers

import (
	"net/http"
)

func chain(h http.Handler, m ...middleware) http.Handler {
	for _, m := range m {
		h = m(h)
	}
	return h
}

type middleware func(http.Handler) http.Handler
