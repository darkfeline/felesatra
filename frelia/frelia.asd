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
               (:file "macro"
                :depends-on ("packages"))
               (:file "resource"
                :depends-on ("packages"))

               (:file "felesatra-macro"
                :depends-on ("packages"))

               (:file "site/site-metadata"
                :depends-on ("packages"))
               (:file "site/resources"
                :depends-on ("packages"))
               (:file "site/resource-loading"
                :depends-on ("packages")))
  :depends-on (:alexandria
               :cl-fad)
  :in-order-to ((test-op (test-op frelia-test))))

(defsystem frelia-test
  :depends-on (:frelia
               :prove)
  :defsystem-depends-on (:prove-asdf)
  :components ((:file "test/packages")
               (:test-file "test/xmllib"
                :depends-on ("test/packages"))
               (:test-file "test/xml"
                :depends-on ("test/packages"))
               (:test-file "test/html"
                :depends-on ("test/packages"))
               (:test-file "test/templates"
                :depends-on ("test/packages"))
               (:test-file "test/macro"
                :depends-on ("test/packages")))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
