(load "load.lisp")
(use-package 'html-tags)
(use-package 'templates)

(defun main ()
  "Entry point."
  (format t "~a~%" (templates::base-template-header))
  (format t "~a~%" (templates::base-template-footer))
  (format t "~a~%" (html-base-template
          :title-block ("HELLO")
          :body-block ((p "how are you"))))
  (format t "~a~%" (base-template
                    :title-block ("HELLO")
                    :body-block ((p "how are you")))))

(main)
