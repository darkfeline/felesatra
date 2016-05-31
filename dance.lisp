(load "setup.lisp")
(asdf:load-system :frelia)

(load "macros.lisp")
(load "templates.lisp")

(defun build-site (&key site-url target-dir)
  (let ((loader (make-instance 'frelia-resource:resource-loader
                               :root-path "site/"))
        (renderer (make-instance 'frelia-resource:resource-renderer
                                 :context `(:site-url ,site-url)
                                 :macro-renderer *macro-renderer*
                                 :template-renderer *template-renderer*)))
    (frelia-resource:load-resources loader)
    (let ((resources (frelia-resource:resources loader)))
      (print resources)
      (frelia-resource:render-resources renderer resources target-dir))))

(defun main ()
  "Entry point."
  (build-site :site-url "https://www.felesatra.moe"
              :target-dir "build-frelia/")
  (build-site :site-url "http://localhost:5000"
              :target-dir "build-frelia-local/"))

(main)
