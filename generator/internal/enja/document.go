package enja

import (
	"bufio"
	"bytes"
	"io"
	"io/ioutil"

	yaml "gopkg.in/yaml.v2"
)

type Document struct {
	Header map[string]interface{}
	Body   []byte
}

const (
	divider = "---\n"
)

func Encode(w io.Writer, d *Document) error {
	bw := bufio.NewWriter(w)
	data, err := yaml.Marshal(d.Header)
	if err != nil {
		return err
	}
	bw.Write(data)
	bw.Write([]byte(divider))
	bw.Write(d.Body)
	return bw.Flush()
}

func Decode(r io.Reader) (*Document, error) {
	var b bytes.Buffer
	br := bufio.NewReader(r)
	for {
		l, err := br.ReadBytes('\n')
		if err != nil {
			return nil, err
		}
		if string(l) == divider {
			break
		}
		b.Write(l)
	}
	h := make(map[string]interface{})
	if err := yaml.Unmarshal(b.Bytes(), h); err != nil {
		return nil, err
	}
	bd, err := ioutil.ReadAll(br)
	if err != nil {
		return nil, err
	}
	d := Document{
		Header: h,
		Body:   bd,
	}
	return &d, nil
}
