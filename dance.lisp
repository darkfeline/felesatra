(load "setup.lisp")
(asdf:load-system :frelia)

(load "macros.lisp")

(defun main ()
  "Entry point."
  (let ((loader (make-instance 'frelia-resource:resource-loader
                               :root-path "site/"))
        (renderer (make-instance 'frelia-resource:resource-renderer
                                 :context '(:site-url "https://www.felesatra.moe")
                                 :macro-renderer *macro-renderer*)))
    (frelia-resource:load-resources loader)
    (print (frelia-resource:resources loader))
    (frelia-resource:render-resources renderer "build-frelia/")))

(main)
