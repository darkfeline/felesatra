package enja

import (
	"reflect"
	"strings"
	"testing"
)

func TestDecode(t *testing.T) {
	t.Parallel()
	const s = `kouka: lacia
---
amadeus
`
	d, err := Decode(strings.NewReader(s))
	if err != nil {
		t.Fatalf("Decode returned error: %s", err)
	}
	want := Document{
		Header: map[string]string{
			"kouka": "lacia",
		},
		Body: []byte("amadeus\n"),
	}
	if !reflect.DeepEqual(d, want) {
		t.Errorf("Expected %#v, got %#v", want, d)
	}
}
