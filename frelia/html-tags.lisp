(in-package :html-tags)

(export 'doctype)
(defun doctype ()
  "HTML5 doctype"
  "<!DOCTYPE html>")

(defmacro deftag (name)
  "Define an HTML tag as a function."
  `(defun ,name (&optional attrs &rest content)
     (unless (listp attrs)
       (setf content (append (list attrs) content))
       (setf attrs nil))
     (format-tag
      (make-xml-tag :name ,(string-downcase (symbol-name name))
                    :attrs attrs
                    :content content))))

(defmacro defvoidtag (name)
  "Define a void HTML tag as a function."
  `(defun ,name (&optional attrs)
     (format-void-tag
      (make-xml-tag :name ,(string-downcase (symbol-name name))
                    :attrs attrs
                    :content '()))))

(defmacro deftags (tag-type tags)
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
                 span
                 title
                 ul
                 ))

(deftags defvoidtag (
                     img
                     link
                     meta
                     ))
