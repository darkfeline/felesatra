(load "setup.lisp")
(asdf:load-system :frelia)

(defun main ()
  "Entry point."
  (let ((loader (make-instance 'frelia-resource:resource-loader
                               :root-path "site/")))
    (frelia-resource:load-resources loader)
    (print (frelia-resource:resources loader))))

(main)
