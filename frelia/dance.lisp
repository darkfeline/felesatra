(load "load.lisp")
(use-package 'html-tags)
(use-package 'templates)

(defun main ()
  "Entry point."
  (format t "~a~%" (base-template
                    :title "HELLO"
                    :body-block (list (p "how are you"))))
  (format t "~a~%" (content-page-template
                    :title "HELLO"
                    :metadata (make-page-metadata :published "test")
                    :content-block (list (p "how are you"))))
  )

(main)
