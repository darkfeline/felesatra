(load "~/quicklisp/setup.lisp")
(ql:quickload :alexandria)
(ql:quickload :cl-fad)

(require :asdf)
(asdf:load-system :frelia)

(defun main ()
  "Entry point."
  (let* ((resources (frelia-site:load-resources "site/"))
         (context
           (make-instance
            'frelia-site:rendering-context
            :site-metadata (make-instance
                            'frelia-site:site-metadata
                            :url "https://www.felesatra.moe/")
            :resources resources))
         (local-context
           (make-instance
            'frelia-site:rendering-context
            :site-metadata (make-instance
                            'frelia-site:site-metadata
                            :url "http://localhost:5000/")
            :resources resources)))
    (frelia-site:render-resources context resources "build/")
    (frelia-site:render-resources local-context resources "build_local/")))

(main)
