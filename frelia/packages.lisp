(in-package "CL-USER")

(defpackage "FRELIA-UTILS"
  (:use "CL")
  (:export "STRING-JOIN"
           "FLATTEN-STRING"))

(defpackage "FRELIA-XML"
  (:use "CL" "FRELIA-UTILS")
  (:export "RENDER-XML"
           "RENDER-XML-DECL"
           "RENDER-XML-WITH-DECL"))

(defpackage "FRELIA-HTML"
  (:use "CL"
        "FRELIA-XML"
        "FRELIA-UTILS"))

(defpackage "FRELIA-SITE"
  (:use "CL"
        "FRELIA-UTILS")
  (:export "SITE-URL"))

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
