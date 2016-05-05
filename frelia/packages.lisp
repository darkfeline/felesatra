(in-package "CL-USER")

(defpackage "FRELIA-UTILS"
  (:use "CL")
  (:export "FLATTEN"
           "STRING-JOIN"
           "FLATTEN-STRING"))

(defpackage "FRELIA-XML"
  (:use "CL")
  (:import-from "FRELIA-UTILS"
                "STRING-JOIN")
  (:export "XML-TAG"
           "FORMAT-TAG"
           "FORMAT-EMPTY-TAG"
           "FORMAT-VOID-TAG"
           "XML-DECL"))

(defpackage "FRELIA-HTML"
  (:use "CL"
        "FRELIA-XML")
  (:import-from "FRELIA-UTILS"
                "FLATTEN-STRING"))

(defpackage "FRELIA-SITE"
  (:use "CL"
        "FRELIA-HTML"
        "COM.GIGAMONKEYS.PATHNAMES")
  (:import-from "FRELIA-UTILS"
                "STRING-JOIN"))

(defpackage "FRELIA-SITE-MACROS"
  (:use "CL"
        "FRELIA-HTML")
  (:import-from "FRELIA-UTILS"
                "STRING-JOIN")
  (:export "ABS-URL"
           "PGP-KEY-A"
           "SITE-A"))

(defpackage "FRELIA-TEMPLATES"
  (:use "CL"
        "FRELIA-HTML")
  (:import-from "FRELIA-UTILS"
                "STRING-JOIN")
  (:import-from "FRELIA-SITE-MACROS"
                "ABS-URL"
                "PGP-KEY-A"
                "SITE-A")
  (:export "HTML-BASE-TEMPLATE"
           "BASE-TEMPLATE"
           "CONTENT-PAGE-TEMPLATE"))
