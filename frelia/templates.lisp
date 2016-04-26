(in-package :templates)

(defmacro html-base-template (&key head-block title-block body-block)
  "HTML base template."
  `(string-join
    (list (doctype)
          (html :lang "en"
                (head
                 (meta :charset "UTF-8")
                 ,@head-block
                 (title ,@title-block))
                (body ,@body-block)))))
