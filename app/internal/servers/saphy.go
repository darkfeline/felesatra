package servers

import "net/http"

func NewSaphy(f CheckFunc) http.Handler {
	return chain(
		pageServer{fs: http.Dir("srv/saphy")},
		withCompress,
		withBasicAuth("yggdrasil", f),
	)
}
