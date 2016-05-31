(in-package :cl-user)

(defpackage :frelia-xmllib
  (:use :cl)
  (:export #:format-attrs
           #:render-content
           #:with-element))

(defpackage :frelia-xml
  (:use :cl :frelia-xmllib)
  (:export #:render-xml
           #:render-xml-element))

(defpackage :frelia-html
  (:use :cl :frelia-xmllib)
  (:export #:render-html
           #:render-html-element))

(defpackage :frelia-templates
  (:use :cl)
  (:export #:html-base-template
           #:base-template
           #:content-page-template))

(defpackage :frelia-macro
  (:use :cl)
  (:export #:macro-renderer
           #:register-macro
           #:render-macros))

(defpackage :frelia-resource
  (:use :cl)
  (:export #:resource-loader
           #:load-resources
           #:resources
           #:resource-renderer
           #:render-resources))
