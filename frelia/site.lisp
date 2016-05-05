(in-package "frelia-site")

(defstruct site-metadata
  url)

(defstruct page-metadata
  "Page metadata."
  path
  template
  content
  published
  modified
  category
  tags)

(defun build-site (site))
