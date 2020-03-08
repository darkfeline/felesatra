package servers

import "net/http"

func withCacheControl(h http.Handler, cacheControl ...string) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
		h.ServeHTTP(&cacheWriter{
			ResponseWriter: w,
			cacheControl:   cacheControl,
		}, req)
	})
}

// cacheWriter implements a http.ResponseWriter that adds a
// Cache-Control header when writing 200 OK responses.
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
	if c == http.StatusOK {
		w.ResponseWriter.Header()["Cache-Control"] = w.cacheControl
	}
	w.ResponseWriter.WriteHeader(c)
}
