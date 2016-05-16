(in-package :cl-user)

(defpackage :frelia-xmllib
  (:use :cl)
  (:export #:collect-attrs
           #:collect-content
           #:make-element
           #:element-name
           #:element-attrs
           #:element-content
           #:with-element))

(defpackage :frelia-xml
  (:use :cl :frelia-xmllib)
  (:export #:render-xml
           #:render-xml-element))

(defpackage :frelia-html
  (:use :cl :frelia-xmllib)
  (:export #:render-html
           #:render-html-element))

(defpackage :frelia-site
  (:use :cl)
  (:export #:load-resources
           #:render-resources
           #:rendering-context
           #:site-metadata))

(defpackage :frelia-templates
  (:use :cl)
  (:export #:html-base-template
           #:base-template
           #:content-page-template))
