package main

import (
	"testing"

	"kanade/internal/index"

	"github.com/google/go-cmp/cmp"
)

func TestProcessIndexEntries(t *testing.T) {
	t.Parallel()
	e := []index.Entry{
		{Path: "blog/a"},
		{Path: "blog/z"},
		{Path: "plachta"},
		{Path: "lacia"},
	}
	got := sortIndexEntries(e)
	want := []index.Entry{
		{Path: "lacia"},
		{Path: "plachta"},
		{Path: "blog/z"},
		{Path: "blog/a"},
	}
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("entries mismatch (-want +got):\n%s", diff)
	}
}
