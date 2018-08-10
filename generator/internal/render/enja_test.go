package render

import (
	"testing"

	"go.felesatra.moe/felesatra/generator/internal/enja"
)

func TestSetMissingHeaders(t *testing.T) {
	t.Parallel()
	d := enja.Document{
		Header: map[string]interface{}{},
	}
	setMissingHeaders(&d, "blog/2016/01/02/some-page")
	t.Run("published", func(t *testing.T) {
		t.Parallel()
		i, ok := d.Header["published"]
		if !ok {
			t.Fatalf("header not set")
		}
		v, ok := i.(string)
		if !ok {
			t.Fatalf("header value not string")
		}
		exp := "2016-01-02"
		if v != exp {
			t.Errorf("Expected %#v, got %#v", exp, v)
		}
	})
}
