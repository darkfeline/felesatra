(in-package :cl-user)

(defpackage :utils
  (:use :cl)
  (:export :string-join))

(defpackage :xml
  (:use :cl)
  (:import-from :utils :string-join)
  (:export :deftag :parse-xml-tag :format-tag :format-empty-tag))

(defpackage :html-tags
  (:use :cl)
  (:import-from :xml :deftag)
  (:export :doctype :html :head :meta :title
           :body :a :p))

(defpackage :templates
  (:use :cl :html-tags)
  (:import-from :utils :string-join)
  (:export :html-base-template))
