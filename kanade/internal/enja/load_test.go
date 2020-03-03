package enja

import (
	"testing"
)

func TestSetMissingHeaders(t *testing.T) {
	t.Parallel()
	d := Document{
		Header: map[string]string{},
	}
	setMissingHeaders(&d, "blog/2016/01/02/some-page")
	t.Run("published", func(t *testing.T) {
		t.Parallel()
		got := d.Header["published"]
		want := "2016-01-02"
		if got != want {
			t.Errorf("Expected %#v, got %#v", want, got)
		}
	})
}
