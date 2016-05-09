(in-package "FRELIA-XML")

(defun render-xml-element (element)
  "Render XML element.

ELEMENT is (NAME ATTRIBUTES &rest CONTENT).  NAME is a string.  ATTRIBUTES is an
alist mapping strings to strings.  CONTENT are strings or further ELEMENTs."
  (let ((name (first element))
        (attributes (second element))
        (content (cddr element)))
    (format nil "<~A~{~^ ~A~}>~{~A~}</~A>"
            name
            (collect-attrs attributes)
            (collect-content 'render-xml-element content)
            name)))

(defun render-xml-decl (attributes)
  "Format an XML declaration with the given attributes."
  (format nil "<?xml~{~^ ~A~}?>"
          (collect-attrs attributes)))

(defun render-xml (attributes root-element)
  "Render XML document."
  (format nil "~A~A"
          (render-xml-decl attributes)
          (render-xml-element root-element)))
