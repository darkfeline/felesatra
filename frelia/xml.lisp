(in-package "FRELIA-XML")

(defun collect-attrs (attributes)
  (loop
    for (key . value) in attributes
    collect (format nil "~A=\"~A\"" key value)))

(defun render-xml (xml-tag)
  "Render XML.

XML-TAG is (TAG-NAME ATTRIBUTES &REST CONTENT).  TAG-NAME is a string.
ATTRIBUTES is an alist mapping strings to strings.  CONTENT are strings or
further XML-TAGs."
  (let* ((tag-name (first xml-tag))
         (xml-tag (rest xml-tag))
         (attributes (first xml-tag))
         (content (rest xml-tag)))
    (format nil "<~A~{~^ ~A~}>~{~A~}</~A>"
            tag-name
            (collect-attrs attributes)
            (loop
              for item in content
              collect (cond
                        ((listp item) (render-xml item))
                        (t item)))
            tag-name)))

(defun render-xml-decl (attributes)
  "Format an XML declaration with the given attributes."
  (format nil "<?xml~{~^ ~A~}?>"
          (collect-attrs attributes)))

(defun render-xml-with-decl (attributes xml-tag)
  "Render XML with an XML declaration."
  (format nil "~A~A" (render-xml-decl attributes) (render-xml xml-tag)))
