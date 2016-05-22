(in-package :frelia-site)

(defun render-footnotes (root-element)
  "Replace footnote references."
  (let ((footnote-index 1))
    (flet ((recurse (element)
             (transform-recursively
              (lambda (element)
                )
              element)
             ))
      (recurse root-element))))

(defun render-macros (context element)
  "Expand site-specific macros."
  (cond
    ((stringp element) element)
    ((symbolp element) element)
    ((macro-p element)
     (let* ((macro (get-macro-function element))
            (expanded-element (apply macro context (rest element))))
       (render-macros context expanded-element)))
    ((listp element)
     (loop
       for subelement in element
       collect (render-macros context subelement)))
    (t "")))

(defun macro-p (element)
  (and (listp element)
       (gethash (first element) *site-macros*)))

(defun get-macro-function (element)
  (gethash (first element) *site-macros*))

(defvar *site-macros* (make-hash-table))

(defun register-site-macro (symbol)
  (setf (gethash (alexandria:make-keyword (symbol-name symbol))
                 *site-macros*)
        symbol))

(defmacro defsitemacro (name lambda-list &body body)
  `(progn
     (defun ,name ,lambda-list
       ,@body)
     (register-site-macro (quote ,name))))

(defsitemacro eval-with-data (context lambda-list &rest body)
  "Macro for evaluating arbitrary Lisp with rendering data."
  (destructuring-bind (var) lambda-list
    (funcall (eval `(lambda (,var) ,@body)) context)))

(defsitemacro abs-url (context path)
  "Get absolute URL for site path."
  (concatenate 'string (site-url context) (string-left-trim "/" path)))

(defsitemacro pgp-key-a (context &rest content)
  "<a> tag to my PGP key."
  `(:a :href "https://sks-keyservers.net/pks/lookup?op=get&search=0x871AC6C82D45F74D"
       ,@content))

(defsitemacro site-a (context path &rest content)
  "<a> tag to site path."
  `(:a :href (:abs-url ,path) ,@content))
