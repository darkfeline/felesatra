(in-package "frelia-xml")

(defstruct xml-tag
  "Defines an XML tag.

`name' is a string.
`attrs' is an alist mapping strings to strings.
`content' is a string.

An `xml-tag' roughly corresponds to the following in XML:

<name attr1=\"value\" attr2=\"value\">content</name>

However, this varies depending on the format function used; the above is
produced by `format-tag', but for example `format-empty-tag' will ignore
`content'."
  name
  attrs
  content)

(defun format-attr (attrs)
  "Format XML attributes from alist."
  (apply 'string-join
         (loop
           for (key . value) in attrs
           collect (format nil " ~A=\"~A\"" key value))))

(defun format-tag (tag)
  "Format `xml-tag' as an XML tag."
  (string-join
   "<" (xml-tag-name tag) (format-attr (xml-tag-attrs tag)) ">"
   (xml-tag-content tag)
   "</" (xml-tag-name tag) ">"))

(defun format-empty-tag (tag)
  "Format `xml-tag' as an empty XML tag."
  (string-join
   "<" (xml-tag-name tag) (format-attr (xml-tag-attrs tag)) "/>"))

(defun format-void-tag (tag)
  "Format `xml-tag' as a void HTML tag."
  (string-join
   "<" (xml-tag-name tag) (format-attr (xml-tag-attrs tag)) ">"))

(defun format-xml-decl-tag (tag)
  "Format `xml-tag' as an XML declaration tag."
  (string-join
   "<?" (xml-tag-name tag) (format-attr (xml-tag-attrs tag)) "?>"))

(defun xml-decl (attrs)
  "Format an XML declaration with the given attributes."
  (format-xml-decl-tag (make-xml-tag :name "xml"
                                     :attrs attrs
                                     :content '())))
