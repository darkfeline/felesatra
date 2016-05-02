(in-package "CL-USER")

(defpackage "UTILS"
  (:use "CL")
  (:export "FLATTEN" "STRING-JOIN" "FLATTEN-STRING"))

(defpackage "XML"
  (:use "CL")
  (:import-from "UTILS" "STRING-JOIN")
  (:export
   "MAKE-XML-TAG"
   "FORMAT-TAG" "FORMAT-EMPTY-TAG" "FORMAT-VOID-TAG" "FORMAT-DECL-TAG"))

(defpackage "HTML-TAGS"
  (:use "CL" "XML")
  (:import-from "UTILS" "FLATTEN-STRING"))

(defpackage "SITE"
  (:use "CL" "HTML-TAGS")
  (:import-from "UTILS" "STRING-JOIN")
  (:export "ABS-URL" "PGP-KEY-A" "SITE-A"))

(defpackage "TEMPLATES"
  (:use "CL" "HTML-TAGS")
  (:import-from "UTILS" "STRING-JOIN")
  (:import-from "SITE" "ABS-URL" "PGP-KEY-A" "SITE-A")
  (:export "HTML-BASE-TEMPLATE" "BASE-TEMPLATE"
           "CONTENT-PAGE-TEMPLATE"
           "MAKE-PAGE-METADATA"))
