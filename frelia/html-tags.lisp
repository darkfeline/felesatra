(in-package :html-tags)

(defun doctype ()
  "HTML5 doctype"
  "<!DOCTYPE html>")

(deftag html)

(deftag head)
(deftag meta :empty t)
(deftag title)

(deftag body)
(deftag p)
(deftag a)
