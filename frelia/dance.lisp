(load "load.lisp")
(use-package 'html-tags)
(use-package 'templates)

(defun main ()
  (print (html-base-template
          :title-block ("HELLO")
          :body-block ((p "how are you")))))

(main)
