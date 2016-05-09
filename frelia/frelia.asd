(defpackage "FRELIA-ASD"
  (:use "CL" "ASDF"))

(in-package "FRELIA-ASD")

(defsystem frelia
  :components ((:file "packages")
               (:file "xmllib"
                :depends-on ("packages"))
               (:file "xml"
                :depends-on ("packages"))
               (:file "html"
                :depends-on ("packages"))
               (:file "site-metadata"
                :depends-on ("packages"))
               (:file "site-macros"
                :depends-on ("packages"))
               (:file "templates"
                :depends-on ("packages"
                             "site-macros")))
  :depends-on ("alexandria" "cl-fad"))
