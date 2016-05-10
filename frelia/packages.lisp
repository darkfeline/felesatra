(in-package "CL-USER")

(defpackage "FRELIA-XMLLIB"
  (:use "CL")
  (:export "COLLECT-ATTRS"
           "COLLECT-CONTENT"
           "WITH-ELEMENT"))

(defpackage "FRELIA-XML"
  (:use "CL" "FRELIA-XMLLIB")
  (:export "RENDER-XML"
           "RENDER-XML-ELEMENT"))

(defpackage "FRELIA-HTML"
  (:use "CL" "FRELIA-XMLLIB")
  (:export "RENDER-HTML"
           "RENDER-HTML-ELEMENT"))

(defpackage "FRELIA-SITE"
  (:use "CL")
  (:export "LOAD-RESOURCES"
           "RENDER-RESOURCES"
           "RENDERING-CONTEXT"
           "SITE-METADATA"))

(defpackage "FRELIA-TEMPLATES"
  (:use "CL")
  (:export "HTML-BASE-TEMPLATE"
           "BASE-TEMPLATE"
           "CONTENT-PAGE-TEMPLATE"))
