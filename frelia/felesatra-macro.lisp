(in-package :frelia-felesatra-macro)

(defvar *felesatra-renderer*
  (make-instance 'macro-renderer))

(defmacro defsitemacro (name lambda-list &body body)
  `(register-macro *felesatra-renderer*
                        ,(alexandria:make-keyword (symbol-name name))
                        (lambda ,lambda-list ,@body)))

(defsitemacro eval-with-context (context lambda-list &rest body)
  "Macro for evaluating arbitrary Lisp with rendering context."
  (destructuring-bind (var) lambda-list
    (funcall (eval `(lambda (,var) ,@body)) context)))

(defsitemacro abs-url (context path)
  "Get absolute URL for site path."
  (concatenate 'string (gethash :site-url context) (string-left-trim "/" path)))

(defsitemacro pgp-key-a (context &rest content)
  "<a> tag to my PGP key."
  `(:a :href "https://sks-keyservers.net/pks/lookup?op=get&search=0x871AC6C82D45F74D"
       ,@content))

(defsitemacro site-a (context path &rest content)
  "<a> tag to site path."
  `(:a :href (:abs-url ,path) ,@content))

(defsitemacro ref (context &rest content)
  "Footnote/ref tag macro."
  (let ((refnum (gethash :refnum context 1)))
    (setf (gethash :refnum context) (+ refnum 1))
    `(:span :class "ref"
            (:span :class "refnum" (format nil "[~A]" refnum))
            (:span :class "refbody" :style "display: none;" ,@content))))
