(in-package "CL-USER")

(defpackage "FRELIA-UTILS"
  (:use "CL")
  (:export "STRING-JOIN"
           "FLATTEN-STRING"))

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
           "SITE-URL"))

(defpackage "FRELIA-TEMPLATES"
  (:use "CL"
        "FRELIA-UTILS"
        "FRELIA-SITE-MACROS"
        "FRELIA-HTML")
  (:export "HTML-BASE-TEMPLATE"
           "BASE-TEMPLATE"
           "CONTENT-PAGE-TEMPLATE"))
