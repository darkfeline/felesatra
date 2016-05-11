(in-package :frelia-site)

(defvar *site-macros* (make-hash-table))

(defun make-keyword (symbol)
  (intern (symbol-name symbol) "KEYWORD"))

(defmacro defsitemacro (name lambda-list &body body)
  `(progn
     (defun ,name ,lambda-list
       ,@body)
     (setf (gethash ,(make-keyword name) *site-macros*)
           (quote ,name))))

(defun prerender-html (context element)
  "Prerender HTML by expanding site-specific macros."
  (render-macros context element))

(defun render-macros (context element)
  "Expand site-specific macros."
  (cond
    ((stringp element) element)
    ((symbolp element) element)
    ((and (listp element)
          (gethash (first element) *site-macros*))
     (let ((macro (gethash (first element) *site-macros*)))
       (render-macros context
                      (apply macro context (rest element)))))
    ((listp element)
     (loop
       for subelement in element
       collect (render-macros context subelement)))
    (t "")))

(defsitemacro eval-with-data (context lambda-list &rest body)
  "Macro for evaluating arbitrary Lisp with rendering data."
  (destructuring-bind (var) lambda-list
    (eval `(funcall (lambda (,var) ,@body) ,context))))

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
