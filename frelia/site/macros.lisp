(in-package :frelia-site)

(defun transform-recursively (transform-function element)
  "Transform an element tree recursively.

Used for applying macros and other transformations to an XML-style tree."
  (cond
    ((stringp element) element)
    ((symbolp element) element)
    ((listp element)
     (transform-function element))
    (t "")))

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
    ((and (listp element)
          (gethash (first element) *site-macros*))
     (let ((macro (gethash (first element) *site-macros*)))
       (render-macros context
                      (apply macro context (rest element)))))
    ((listp element)
     (loop
       for subelement in element
       collect (render-macros context subelement)))
    (t ""))
  (transform-recursively
   (lambda (element)
     (let ((macro (gethash (first element) *site-macros*)))
       (if macro
           (render-macros context
                          (apply macro context (rest element)))
           (loop
             for subelement in element
             collect (render-macros context subelement)))))
   element))

(defvar *site-macros* (make-hash-table))

(defmacro defsitemacro (name lambda-list &body body)
  `(progn
     (defun ,name ,lambda-list
       ,@body)
     (setf (gethash ,(alexandria:make-keyword (symbol-name name))
                    *site-macros*)
           (quote ,name))))

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