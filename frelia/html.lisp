(in-package "FRELIA-HTML")

(export 'doctype)
(defun doctype ()
  "HTML5 doctype"
  "<!DOCTYPE html>")

(defmacro deftag (name)
  "Define an HTML tag as a function."
  `(defun ,name (&optional attrs &rest content)
     (unless (listp attrs)
       (setf content (cons attrs content))
       (setf attrs nil))
     (format-tag
      (make-instance xml-tag
                     :name ,(string-downcase (symbol-name name))
                     :attrs attrs
                     :content (flatten-string content)))))

(defmacro defvoidtag (name)
  "Define a void HTML tag as a function."
  `(defun ,name (&optional attrs)
     (format-void-tag
      (make-instance xml-tag
                     :name ,(string-downcase (symbol-name name))
                     :attrs attrs))))

(defmacro deftags (tag-type tags)
  "Define and export a list of tags using the given deftag macro."
  (append
   '(progn)
   (loop for tag in tags
         append
         `((,tag-type ,tag)
           (export (quote ,tag))))))

(deftags deftag (
                 a
                 address
                 body
                 dd
                 div
                 dl
                 dt
                 footer
                 h1
                 head
                 header
                 html
                 li
                 ol
                 p
                 section
                 span
                 title
                 ul
                 ))

(deftags defvoidtag (
                     img
                     link
                     meta
                     ))
