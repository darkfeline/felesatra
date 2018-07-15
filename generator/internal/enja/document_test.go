package enja

import (
	"bytes"
	"reflect"
	"strings"
	"testing"
)

func TestEncode(t *testing.T) {
	t.Parallel()
	d := Document{
		Header: map[string]interface{}{
			"kouka": "lacia",
		},
		Body: []byte("amadeus\n"),
	}
	var b bytes.Buffer
	if err := Encode(&b, &d); err != nil {
		t.Fatalf("Encode returned error: %s", err)
	}
	exp := `kouka: lacia
---
amadeus
`
	if b.String() != exp {
		t.Errorf("Expected %#v, got %#v", exp, b.String())
	}
}

func TestDecode(t *testing.T) {
	t.Parallel()
	s := `kouka: lacia
---
amadeus
`
	d, err := Decode(strings.NewReader(s))
	if err != nil {
		t.Fatalf("Decode returned error: %s", err)
	}
	exp := Document{
		Header: map[string]interface{}{
			"kouka": "lacia",
		},
		Body: []byte("amadeus\n"),
	}
	if !reflect.DeepEqual(d, &exp) {
		t.Errorf("Expected %#v, got %#v", &exp, d)
	}
}
