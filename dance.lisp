(load "~/quicklisp/setup.lisp")
(ql:quickload "alexandria")
(ql:quickload "cl-fad")

(require "asdf")
(asdf:load-system "frelia")

(defun main ()
  "Entry point.")

(main)
