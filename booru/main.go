package main

import (
	"crypto/sha256"
	"fmt"
	"io/ioutil"
	"log"
	"math"
	"net/http"
	"path"

	"google.golang.org/appengine"
	"google.golang.org/appengine/datastore"

	"go.felesatra.moe/felesatra/internal/booru/dstype"
)

func main() {
	http.HandleFunc("/images/", handleImages)
	http.HandleFunc("/", handleDefault)
	appengine.Main()
}

func handleImages(w http.ResponseWriter, r *http.Request) {
	if r.URL.Path == "/images/" {
		handleImagesRoot(w, r)
	} else {
		handleImagesPath(w, r)
	}
}

func handleImagesRoot(w http.ResponseWriter, r *http.Request) {
	switch r.Method {
	case "GET":
		for _, t := range templ.Templates() {
			fmt.Fprintf(w, "%#v\n", t.Name())
		}
		templ.ExecuteTemplate(w, "images-root.html", nil)
	case "POST":
		handleImagesRootPost(w, r)
	default:
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
	}
}

var fileMem = int64(4 * math.Pow(2, 20))

func handleImagesRootPost(w http.ResponseWriter, r *http.Request) {
	var err error
	defer func() {
		if err != nil {
			log.Print(err)
			http.Error(w, err.Error(), http.StatusBadRequest)
		}
	}()
	if err = r.ParseMultipartForm(fileMem); err != nil {
		return
	}
	fh := r.MultipartForm.File["file"]
	if fh == nil || len(fh) == 0 {
		err = fmt.Errorf("Missing file part")
		return
	}
	f, err := fh[0].Open()
	if err != nil {
		return
	}
	defer f.Close()
	data, err := ioutil.ReadAll(f)
	if err != nil {
		return
	}
	sum := sha256.Sum256(data)
	id := fmt.Sprintf("%x", sum)
	ctx := appengine.NewContext(r)
	k := datastore.NewKey(ctx, "Image", id, 0, nil)
	e := &dstype.Image{Data: data}
	if _, err = datastore.Put(ctx, k, e); err != nil {
		return
	}
	w.Write([]byte("OK"))
}

func handleImagesPath(w http.ResponseWriter, r *http.Request) {
	switch r.Method {
	case "GET":
		id := path.Base(r.URL.Path)
		ctx := appengine.NewContext(r)
		k := datastore.NewKey(ctx, "Image", id, 0, nil)
		e := &dstype.Image{}
		if err := datastore.Get(ctx, k, e); err != nil {
			http.NotFound(w, r)
			return
		}
		w.Write(e.Data)
	default:
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
	}
}

func handleDefault(w http.ResponseWriter, r *http.Request) {
	templ.ExecuteTemplate(w, "root.html", nil)
}
