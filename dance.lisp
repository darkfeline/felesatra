(load "~/quicklisp/setup.lisp")
(ql:quickload "cl-fad")
(ql:quickload "alexandria")

(require "asdf")
(asdf:load-system "frelia")

(use-package "frelia-html")
(use-package "frelia-templates")

(defun main ()
  "Entry point."
  (format t "~a~%" (p "test"))
  (format t "~a~%" (base-template
                    :title "HELLO"
                    :body-block (p "how are you")))
  (format t "~a~%" (content-page-template
                    :title "HELLO"
                    :metadata (make-page-metadata :published "test")
                    :content-block (p "how are you"))))

(main)
