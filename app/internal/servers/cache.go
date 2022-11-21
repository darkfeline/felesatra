package servers

import "net/http"

func withCacheControl(cacheControl ...string) middleware {
	return func(h http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
			h.ServeHTTP(&cacheWriter{
				ResponseWriter: w,
				cacheControl:   cacheControl,
			}, req)
		})
	}
}

// cacheWriter implements a http.ResponseWriter that adds a
// Cache-Control header when writing cache-able responses.
type cacheWriter struct {
	http.ResponseWriter
	cacheControl  []string
	headerWritten bool
}

func (w *cacheWriter) Write(d []byte) (int, error) {
	if !w.headerWritten {
		w.WriteHeader(http.StatusOK)
	}
	return w.ResponseWriter.Write(d)
}

func (w *cacheWriter) WriteHeader(c int) {
	w.headerWritten = true
	switch c {
	case 200, 302, 303, 307:
		w.ResponseWriter.Header()["Cache-Control"] = w.cacheControl
	}
	w.ResponseWriter.WriteHeader(c)
}
