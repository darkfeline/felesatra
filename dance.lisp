(load "~/quicklisp/setup.lisp")
(ql:quickload "alexandria")
(ql:quickload "cl-fad")

(require "asdf")
(asdf:load-system "frelia")

(defun main ()
  "Entry point."
  (let* ((resources (frelia-site:load-resources "site/"))
         (page-index )))
  )

(main)
