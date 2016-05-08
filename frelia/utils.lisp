(in-package "FRELIA-UTILS")

(defun string-join (&rest strings)
  "Join a list of strings."
  (apply 'concatenate 'string strings))

(defun flatten-string (&rest things)
  "Join nested lists of strings."
  (apply 'string-join (alexandria:flatten things)))
