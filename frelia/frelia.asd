(defpackage "FRELIA-ASD"
  (:use "CL" "ASDF"))

(in-package "FRELIA-ASD")

(defsystem frelia
  :components ((:file "packages")
               (:file "utils"
                :depends-on ("packages"))
               (:file "xml"
                :depends-on ("packages"
                             "utils"))
               (:file "html"
                :depends-on ("packages"
                             "utils"
                             "xml"))
               (:file "site"
                :depends-on ("packages"
                             "utils"
                             "html"))
               (:file "site-macros"
                :depends-on ("packages"
                             "utils"
                             "html"))
               (:file "templates"
                :depends-on ("packages"
                             "utils"
                             "site-macros"
                             "html")))
  :depends-on ("com.gigamonkeys.pathnames"))
