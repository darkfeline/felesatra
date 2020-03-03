// Package enja implements support for documents with simple metadata.
package enja

import (
	"bufio"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"strings"
)

// A Document represents a document with simple metadata.
type Document struct {
	Header map[string]string
	Body   []byte
}

const (
	divider = "---\n"
)

// Decode decodes a Document from the Reader.
func Decode(r io.Reader) (Document, error) {
	d := Document{
		Header: make(map[string]string),
	}
	br := bufio.NewReader(r)
	for {
		line, err := br.ReadBytes('\n')
		if err != nil {
			return d, fmt.Errorf("enja decode: %w", err)
		}
		if string(line) == divider {
			break
		}
		k, v, err := parseHeaderLine(line[:len(line)-1])
		if err != nil {
			return d, fmt.Errorf("enja decode: %s", err)
		}
		d.Header[k] = v
	}
	var err error
	d.Body, err = ioutil.ReadAll(br)
	if err != nil {
		return d, err
	}
	return d, nil
}

func parseHeaderLine(d []byte) (k, v string, err error) {
	for i, c := range d {
		if c == ':' {
			k = string(d[:i])
			v = string(d[i+1:])
			break
		}
	}
	if k == "" {
		return "", "", fmt.Errorf("invalid header line %s", d)
	}
	v = strings.TrimSpace(v)
	if len(v) > 0 && v[0] == '"' {
		if err := json.Unmarshal([]byte(v), &v); err != nil {
			return "", "", fmt.Errorf("invalid header line %s", d)
		}
	}
	return k, v, nil
}
