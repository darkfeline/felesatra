(in-package :xml)

(defstruct xml-tag
  name
  attrs
  content)

(defun parse-xml-tag (name args)
  "Parse XML tag."
  (let (attrs content current-attr)
    (loop
      for arg in args
      do (cond
           (current-attr
            (setf (getf attrs (pop current-attr)) arg))
           ((keywordp arg) (push arg current-attr))
           (t (push arg content))))
    (make-xml-tag :name name
                  :attrs attrs
                  :content (reverse content))))

(defun format-attr (attrs)
  "Format XML attributes from plist."
  (let (strings current-attr)
    (loop
      for arg in attrs
      do (cond
           (current-attr
            (push (format nil " ~A=\"~A\""
                          (string-downcase (symbol-name (pop current-attr)))
                          arg)
                  strings))
           (t (push arg current-attr))))
    (string-join strings)))

(defun format-tag (tag)
  "Format `xml-tag' as an XML tag."
  (string-join
   (list "<" (xml-tag-name tag) (format-attr (xml-tag-attrs tag)) ">"
         (string-join (xml-tag-content tag)) "</" (xml-tag-name tag) ">")))

(defun format-empty-tag (tag)
  "Format `xml-tag' as an empty XML tag."
  (string-join
   (list "<" (xml-tag-name tag) (format-attr (xml-tag-attrs tag)) "/>")))

(defun format-decl-tag (tag)
  "Format `xml-tag' as an XML declaration tag."
  (string-join
   (list "<?" (xml-tag-name tag) (format-attr (xml-tag-attrs tag)) "?>")))

(defun xml-decl (&rest args)
  "XML declaration."
  (format-decl-tag (parse-xml-tag "xml" args)))

(defmacro deftag (name &key empty)
  `(defun ,name (&rest args)
     (,(cond
         (empty 'format-empty-tag)
         (t 'format-tag))
      (parse-xml-tag ,(string-downcase (symbol-name name)) args))))
