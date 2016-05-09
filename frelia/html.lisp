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
  (let* ((element (preprocess-element element))
         (name (first element))
         (attributes (second element))
         (content (cddr element)))
    (if (gethash name *void-elements*)
        (format-void-element name attributes)
        (format-element name attributes content))))

(defun format-element (name attributes content)
  (format nil "<~A~{~^ ~A~}>~{~A~}</~A>"
          name
          (collect-attrs attributes)
          (collect-content 'render-html-element content)
          name))

(defun format-void-element (name attributes)
  (format nil "<~A~{~^ ~A~}>"
          name
          (collect-attrs attributes)))

(defun preprocess-element (element)
  "Preprocess HTML element."
  (let ((name (string-downcase (symbol-name (first element))))
        attributes
        content)
    (loop
      with attr = nil
      for item in (rest element)
      do
         (cond
           (attr
            (push (cons (string-downcase (symbol-name (pop attr)))
                        item)
                  attributes))
           ((symbolp item)
            (push item attr))
           (t
            (push item content))))
    (append (list name attributes) (reverse content))))

(defun render-html (root-element)
  "Render XML document."
  (format nil "~A~A" "<!DOCTYPE html>" (render-html-element root-element)))
