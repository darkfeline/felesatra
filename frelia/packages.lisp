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
  (:export #:render
           #:html-base-template
           #:base-template
           #:content-page-template))

(defpackage :frelia-site
  (:use :cl)
  (:export #:make-site-metadata
           #:rendering-context
           #:load-resources
           #:render-resources))
