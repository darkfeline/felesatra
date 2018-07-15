package index

import (
	"bytes"
	"reflect"
	"testing"
)

func TestReadAndWrite(t *testing.T) {
	var b bytes.Buffer
	e := []Entry{
		{"blog/2000/01/01/plachta", "Plachta", "2000-01-01", "2000-01-02"},
		{"blog/2001/01/01/lacia", "Lacia", "2001-01-01", "2001-01-02"},
	}
	if err := WriteAll(&b, e); err != nil {
		t.Fatalf("Error writing: %s", err)
	}
	got, err := ReadAll(&b)
	if err != nil {
		t.Fatalf("Error reading: %s", err)
	}
	if !reflect.DeepEqual(got, e) {
		t.Errorf("Expected %#v, got %#v", e, got)
	}
}
