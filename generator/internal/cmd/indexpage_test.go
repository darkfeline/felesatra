package cmd

import (
	"reflect"
	"testing"

	"go.felesatra.moe/felesatra/generator/internal/index"
)

func TestProcessIndexEntries(t *testing.T) {
	t.Parallel()
	e := []index.Entry{
		{Path: "blog/a"},
		{Path: "blog/z"},
		{Path: "404"},
		{Path: "plachta"},
		{Path: "lacia"},
	}
	got := processIndexEntries(e)
	exp := []index.Entry{
		{Path: "lacia"},
		{Path: "plachta"},
		{Path: "blog/z"},
		{Path: "blog/a"},
	}
	if !reflect.DeepEqual(got, exp) {
		t.Errorf("Expected %#v, got %#v", exp, got)
	}
}
