package enja

import (
	"bytes"
	"reflect"
	"strings"
	"testing"
)

func TestRead(t *testing.T) {
	t.Parallel()
	d := Document{
		Header: map[string]interface{}{
			"kouka": "lacia",
		},
		Body: []byte("amadeus\n"),
	}
	var b bytes.Buffer
	if err := Write(&b, &d); err != nil {
		t.Fatalf("Write returned error: %s", err)
	}
	exp := `kouka: lacia
---
amadeus
`
	if b.String() != exp {
		t.Errorf("Expected %#v, got %#v", exp, b.String())
	}
}

func TestWrite(t *testing.T) {
	t.Parallel()
	s := `kouka: lacia
---
amadeus
`
	d, err := Read(strings.NewReader(s))
	if err != nil {
		t.Fatalf("Read returned error: %s", err)
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
