(in-package :utils)

(defun string-join (strings)
  "Join a list of strings."
  (apply 'concatenate 'string strings))
