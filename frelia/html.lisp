(in-package "FRELIA-HTML")

(defvar *void-elements*)

(defun setup-void-elements ()
  (setf *void-elements* (make-hash-table :test 'equal))
  (loop
    for element in '("area"
                     "base"
                     "br"
                     "col"
                     "embed"
                     "hr"
                     "img"
                     "input"
                     "keygen"
                     "link"
                     "meta"
                     "param"
                     "source"
                     "track"
                     "wbr")
    do (setf (gethash element *void-elements*) t)))
(setup-void-elements)

(defun render-html-element (element)
  "Render HTML element."
  (with-element (element name attrs content)
    (if (gethash name *void-elements*)
        (format-void-element name attrs)
        (format-element name attrs content))))

(defun format-element (name attrs content)
  (format nil "<~A~{~^ ~A~}>~{~A~}</~A>"
          name
          (collect-attrs attrs)
          (collect-content 'render-html-element content)
          name))

(defun format-void-element (name attrs)
  (format nil "<~A~{~^ ~A~}>"
          name
          (collect-attrs attrs)))

(defun render-html (root-element)
  "Render XML document."
  (format nil "~A~A" "<!DOCTYPE html>" (render-html-element root-element)))
