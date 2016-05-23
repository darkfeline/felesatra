(load "~/quicklisp/setup.lisp")
(ql:quickload :alexandria)
(ql:quickload :cl-fad)
(ql:quickload :prove)

(require :asdf)
(asdf:test-system :frelia)
