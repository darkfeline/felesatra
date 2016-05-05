(in-package "FRELIA-XML")

(defclass xml-tag ()
  ((name :accessor xml-tag-name
         :initarg name)
   (attrs :accessor xml-tag-attrs
          :initarg attrs)
   (content :accessor xml-tag-content
            :initarg content))
  (:documentation
   "Defines an XML tag.

`name' is a string.
`attrs' is an alist mapping strings to strings.
`content' is a string.

An `xml-tag' roughly corresponds to the following in XML:

<name attr1=\"value\" attr2=\"value\">content</name>

However, this varies depending on the format function used; the above is
produced by `format-tag', but for example `format-empty-tag' will ignore
`content'."))

(defmethod format-attrs ((tag xml-tag))
  "Format tag XML attributes."
  (apply 'string-join
         (loop
           for (key . value) in (xml-tag-attrs tag)
           collect (format nil " ~A=\"~A\"" key value))))

(defmethod format-tag ((tag xml-tag))
  "Format `xml-tag' as an XML tag."
  (string-join
   "<" (xml-tag-name tag) (format-attrs tag) ">"
   (xml-tag-content tag)
   "</" (xml-tag-name tag) ">"))

(defmethod format-empty-tag ((tag xml-tag))
  "Format `xml-tag' as an empty XML tag."
  (string-join
   "<" (xml-tag-name tag) (format-attrs tag) "/>"))

(defmethod format-void-tag ((tag xml-tag))
  "Format `xml-tag' as a void HTML tag."
  (string-join
   "<" (xml-tag-name tag) (format-attrs tag) ">"))

(defmethod format-xml-decl-tag ((tag xml-tag))
  "Format `xml-tag' as an XML declaration tag."
  (string-join
   "<?" (xml-tag-name tag) (format-attrs tag) "?>"))

(defun xml-decl (attrs)
  "Format an XML declaration with the given attributes."
  (format-xml-decl-tag
   (make-instance xml-tag :name "xml"
                          :attrs attrs
                          :content "")))
