(in-package :frelia-xml)

(defun render-xml-element (element)
  "Render XML element."
  (with-element (element name attrs content)
    (format nil "<~A~{~^ ~A~}>~{~A~}</~A>"
            name
            (format-attrs attrs)
            (render-content 'render-xml-element content)
            name)))

(defun render-xml-decl (attributes)
  "Format an XML declaration with the given attributes."
  (format nil "<?xml~{~^ ~A~}?>"
          (format-attrs attributes)))

(defun render-xml (attributes root-element)
  "Render XML document."
  (format nil "~A~A"
          (render-xml-decl attributes)
          (render-xml-element root-element)))
