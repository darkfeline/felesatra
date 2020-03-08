package servers

import (
	"testing"
)

func BenchmarkNewGo(b *testing.B) {
	for i := 0; i < b.N; i++ {
		NewGo()
	}
}

func BenchmarkNewGoproxy(b *testing.B) {
	for i := 0; i < b.N; i++ {
		NewGoproxy()
	}
}

func BenchmarkNewFiles(b *testing.B) {
	for i := 0; i < b.N; i++ {
		NewFiles()
	}
}

func BenchmarkNewWeb(b *testing.B) {
	for i := 0; i < b.N; i++ {
		NewWeb(fakeAuth)
	}
}

func fakeAuth(u, pw string) (ok bool) {
	return false
}
