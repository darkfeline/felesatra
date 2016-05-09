(in-package "FRELIA-SITE")

(defvar *site-macros* (make-hash-table))

(defun make-keyword (symbol)
  (intern (symbol-name symbol) "KEYWORD"))

(defmacro defsitemacro (name lambda-list &body body)
  `(progn
     (defun ,name ,lambda-list
       ,@body)
     (setf (gethash ,(make-keyword name) *site-macros*)
           (quote ,name))))

(defun prerender-html (rendering-data element)
  "Prerender HTML by expanding site-specific macros."
  (render-macros rendering-data element))

(defun render-macros (rendering-data element)
  "Expand site-specific macros."
  (cond
    ((stringp element) element)
    ((symbolp element) element)
    ((and (listp element)
          (gethash (first element) *site-macros*))
     (let ((macro (gethash (first element) *site-macros*)))
       (render-macros rendering-data
                      (apply macro rendering-data (rest element)))))
    ((listp element)
     (loop
       for subelement in element
       collect (render-macros rendering-data subelement)))
    (t "")))

(defsitemacro eval-with-data (rendering-data lambda-list &rest body)
  "Macro for evaluating arbitrary Lisp with rendering data."
  (destructuring-bind (var) lambda-list
    (eval `(funcall (lambda (,var) ,@body) ,rendering-data))))

(defsitemacro abs-url (rendering-data path)
  "Get absolute URL for site path."
  (concatenate 'string (site-url rendering-data) (string-left-trim "/" path)))

(defsitemacro pgp-key-a (rendering-data &rest content)
  "<a> tag to my PGP key."
  `(:a :href "https://sks-keyservers.net/pks/lookup?op=get&search=0x871AC6C82D45F74D"
       ,@content))

(defsitemacro site-a (rendering-data path &rest content)
  "<a> tag to site path."
  `(:a :href (:abs-url ,path) ,@content))
