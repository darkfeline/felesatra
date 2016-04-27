(in-package :cl-user)

(defpackage :utils
  (:use :cl)
  (:export :string-join))

(defpackage :xml
  (:use :cl)
  (:import-from :utils :string-join)
  (:export
   :make-xml-tag
   :format-tag :format-empty-tag :format-void-tag :format-decl-tag))

(defpackage :html-tags
  (:use :cl :xml))

(defpackage :site
  (:use :cl :html-tags)
  (:import-from :utils :string-join)
  (:export :abs-url :pgp-key-a :site-a))

(defpackage :templates
  (:use :cl :html-tags)
  (:import-from :utils :string-join)
  (:import-from :site :abs-url :pgp-key-a :site-a)
  (:export :html-base-template :base-template
           :content-page-template))
