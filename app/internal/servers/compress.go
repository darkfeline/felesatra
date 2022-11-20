package servers

import (
	"compress/gzip"
	"net/http"
)

func withCompress(h http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if contains("gzip", r.Header.Values("Accept-Encoding")) {
			w = newGzipWriter(w)
		}
		h.ServeHTTP(w, r)
	})
}

// A gzipWriter wraps an http.ResponseWriter with gzip compression.
type gzipWriter struct {
	http.ResponseWriter
	w *gzip.Writer
}

func newGzipWriter(w http.ResponseWriter) gzipWriter {
	h := w.Header()
	h["Content-Encoding"] = append([]string{"gzip"}, h["Content-Encoding"]...)
	return gzipWriter{
		ResponseWriter: w,
		w:              gzip.NewWriter(w),
	}
}

func (w gzipWriter) Write(d []byte) (int, error) {
	n, err := w.w.Write(d)
	if err != nil {
		_ = w.w.Flush()
		return n, err
	}
	err = w.w.Flush()
	return n, err
}

func contains[K comparable](x K, s []K) bool {
	for _, v := range s {
		if v == x {
			return true
		}
	}
	return false
}
