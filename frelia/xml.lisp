;;;; xml.lisp
;;; Construct XML tags with functions.
(in-package :xml)

(defstruct xml-tag
  "Defines an XML tag.

`name' is a string.
`attrs' is an alist mapping strings to strings.
`content' is a list of strings."
  name
  attrs
  content)

(defun format-attr (attrs)
  "Format XML attributes from alist."
  (let (strings)
    (loop
      for (key . value) in attrs
      do (push (format nil " ~A=\"~A\"" key value)
               strings))
    (apply 'string-join strings)))

(defun format-tag (tag)
  "Format `xml-tag' as an XML tag."
  (string-join
   "<" (xml-tag-name tag) (format-attr (xml-tag-attrs tag)) ">"
   (apply 'string-join (xml-tag-content tag)) "</" (xml-tag-name tag) ">"))

(defun format-empty-tag (tag)
  "Format `xml-tag' as an empty XML tag."
  (string-join
   "<" (xml-tag-name tag) (format-attr (xml-tag-attrs tag)) "/>"))

(defun format-void-tag (tag)
  "Format `xml-tag' as void HTML tag."
  (string-join
   "<" (xml-tag-name tag) (format-attr (xml-tag-attrs tag)) ">"))

(defun format-decl-tag (tag)
  "Format `xml-tag' as an XML declaration tag."
  (string-join
   "<?" (xml-tag-name tag) (format-attr (xml-tag-attrs tag)) "?>"))

(defun xml-decl (attrs)
  "XML declaration."
  (format-decl-tag (make-xml-tag :name "xml"
                                 :attrs attrs
                                 :content '())))
