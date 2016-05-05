(in-package "FRELIA-UTILS")

(defun flatten (l)
  "Flatten a list."
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))

(defun string-join (&rest strings)
  "Join a list of strings."
  (apply 'concatenate 'string strings))

(defun flatten-string (&rest things)
  "Join nested lists of strings."
  (apply 'string-join (flatten things)))
