(in-package "CL-USER")

(defpackage "frelia-utils"
  (:use "CL")
  (:export "FLATTEN"
           "STRING-JOIN"
           "FLATTEN-STRING"))

(defpackage "frelia-xml"
  (:use "CL")
  (:import-from "frelia-utils"
                "STRING-JOIN")
  (:export "MAKE-XML-TAG"
           "FORMAT-TAG"
           "FORMAT-EMPTY-TAG"
           "FORMAT-VOID-TAG"
           "XML-DECL"))

(defpackage "frelia-html"
  (:use "CL"
        "frelia-xml")
  (:import-from "frelia-utils"
                "STRING-JOIN"
                "FLATTEN-STRING"))

(defpackage "frelia-site"
  (:use "CL"
        "frelia-html"
        "COM.GIGAMONKEYS.PATHNAMES")
  (:import-from "frelia-utils" "STRING-JOIN"))

(defpackage "frelia-site-macros"
  (:use "CL"
        "frelia-html")
  (:import-from "frelia-utils"
                "STRING-JOIN")
  (:export "ABS-URL"
           "PGP-KEY-A"
           "SITE-A"))

(defpackage "frelia-templates"
  (:use "CL"
        "frelia-html")
  (:import-from "frelia-utils"
                "STRING-JOIN")
  (:import-from "frelia-site-macros"
                "ABS-URL"
                "PGP-KEY-A"
                "SITE-A")
  (:export "HTML-BASE-TEMPLATE"
           "BASE-TEMPLATE"
           "CONTENT-PAGE-TEMPLATE"))
