(in-package :frelia-macro-test)

(plan 1)
(let ((renderer (make-instance 'macro-renderer))
      (context '(:foo "foo")))
  (flet ((test-macro-function (context arg)
           `(:p ,(getf context :foo) ,arg)))
    (register-macro renderer :test-macro #'test-macro-function)
    (is (render-macros renderer context
                       '(:p "hi" (:test-macro "spam")))
        '(:p "hi" (:p "foo" "spam")))))
(finalize)
