(in-package "CL-USER")

(defpackage "FRELIA-UTILS"
  (:use "CL")
  (:export "STRING-JOIN"
           "FLATTEN-STRING"))

(defpackage "FRELIA-XMLLIB"
  (:use "CL")
  (:export "COLLECT-ATTRS"
           "COLLECT-CONTENT"))

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

(defpackage "FRELIA-SITE-MACROS"
  (:use "CL"
        "FRELIA-UTILS"
        "FRELIA-HTML")
  (:import-from "FRELIA-SITE"
                "SITE-URL")
  (:export "ABS-URL"
           "PGP-KEY-A"
           "SITE-A"))

(defpackage "FRELIA-TEMPLATES"
  (:use "CL"
        "FRELIA-UTILS"
        "FRELIA-SITE-MACROS"
        "FRELIA-HTML")
  (:export "HTML-BASE-TEMPLATE"
           "BASE-TEMPLATE"
           "CONTENT-PAGE-TEMPLATE"))
