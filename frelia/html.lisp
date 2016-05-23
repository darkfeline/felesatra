(in-package :frelia-html)

(defun render-html (root-element)
  "Render XML document."
  (format nil "~A~A" "<!DOCTYPE html>" (render-html-element root-element)))

(defun render-html-element (element)
  "Render HTML element."
  (with-element (element name attrs content)
    (if (void-element-p name)
        (format-void-element name attrs)
        (format-element name attrs content))))

(defparameter *void-elements-list* '("area"
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
                                     "wbr"))
(defvar *void-elements-table* (make-hash-table :test 'equal))

(defun clear-void-elements ()
  (clrhash *void-elements-table*))

(defun register-void-element (element)
  (setf (gethash element *void-elements-table*) t))

(defun register-void-elements (&rest elements)
  (loop
    for element in elements
    do (register-void-element element)))

(defun init-void-elements-table ()
  (clear-void-elements)
  (apply 'register-void-elements *void-elements-list*))
(init-void-elements-table)

(defun void-element-p (element-name)
  "Check if ELEMENT-NAME is a void HTML element."
  (gethash element-name *void-elements-table*))

(defun format-void-element (name attrs)
  (format nil "<~A~{~^ ~A~}>"
          name
          (format-attrs attrs)))

(defun format-element (name attrs content)
  (format nil "<~A~{~^ ~A~}>~{~A~}</~A>"
          name
          (format-attrs attrs)
          (render-content 'render-html-element content)
          name))
