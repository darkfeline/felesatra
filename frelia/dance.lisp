(load "load.lisp")
(use-package 'html-tags)
(use-package 'templates)

(defun main ()
  "Entry point."
  (format t "~a~%" (base-template
                    :title "HELLO"
                    :body-block ((p "how are you")))))

(main)
