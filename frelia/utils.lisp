;;;; utils.lisp
;;; Shared utilities

(in-package :utils)

(defun string-join (&rest strings)
  "Join a list of strings."
  (apply 'concatenate 'string strings))
