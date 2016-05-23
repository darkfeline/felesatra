(defpackage :frelia-asd
  (:use :cl :asdf))

(in-package :frelia-asd)

(defsystem frelia
  :components ((:file "packages")
               (:file "xmllib"
                :depends-on ("packages"))
               (:file "xml"
                :depends-on ("packages"))
               (:file "html"
                :depends-on ("packages"))

               (:file "templates"
                :depends-on ("packages"))

               (:file "site/site-metadata"
                :depends-on ("packages"))
               (:file "site/resources"
                :depends-on ("packages"))
               (:file "site/resource-loading"
                :depends-on ("packages"))
               (:file "site/rendering"
                :depends-on ("packages"))
               (:file "site/macros"
                :depends-on ("packages")))
  :depends-on (:alexandria
               :cl-fad)
  :in-order-to ((test-op (test-op frelia-test))))

(defsystem frelia-test
  :depends-on (:frelia
               :prove)
  :defsystem-depends-on (:prove-asdf)
  :components ((:file "test-packages")
               (:test-file "xmllib-test"
                :depends-on ("test-packages"))
               (:test-file "xml-test"
                :depends-on ("test-packages"))
               (:test-file "html-test"
                :depends-on ("test-packages")))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
