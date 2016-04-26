(in-package :cl-user)

(defpackage :utils
  (:use :cl)
  (:export :string-join))

(defpackage :html
  (:use :cl)
  (:import-from :utils :string-join)
  (:export :deftag))

(defpackage :html-tags
  (:use :cl)
  (:import-from :html :deftag)
  (:export :html :head :meta :title
           :body :a :p))

(defpackage :templates
  (:use :cl :html-tags)
  (:export :html-base-template))
