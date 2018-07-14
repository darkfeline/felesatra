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
	divider  = "---"
	dividerN = "---\n"
)

func Write(w io.Writer, d *Document) error {
	bw := bufio.NewWriter(w)
	data, err := yaml.Marshal(d.Header)
	if err != nil {
		return err
	}
	bw.Write(data)
	bw.Write([]byte(dividerN))
	bw.Write(d.Body)
	return bw.Flush()
}

func Read(r io.Reader) (*Document, error) {
	var b bytes.Buffer
	s := bufio.NewScanner(r)
	for s.Scan() {
		if s.Text() == divider {
			break
		}
		io.WriteString(&b, s.Text())
	}
	if err := s.Err(); err != nil {
		return nil, err
	}
	h := make(map[string]interface{})
	if err := yaml.Unmarshal(b.Bytes(), h); err != nil {
		return nil, err
	}
	bd, err := ioutil.ReadAll(r)
	if err != nil {
		return nil, err
	}
	d := Document{
		Header: h,
		Body:   bd,
	}
	return &d, nil
}
