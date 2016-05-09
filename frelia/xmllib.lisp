(in-package "FRELIA-XMLLIB")

(defun collect-attrs (attributes)
  (loop
    for (key . value) in attributes
    collect (format nil "~A=\"~A\"" key value)))

(defun collect-content (render-function content)
  (loop
    for item in content
    collect (cond
              ((listp item) (funcall render-function item))
              ((stringp item) item)
              (t ""))))

(defun preprocess-force (element)
  (let ((name (string-downcase (symbol-name (first element))))
        attributes
        content)
    (loop
      with attr = nil
      for item in (rest element)
      do
         (cond
           (attr
            (push (cons (string-downcase (symbol-name (pop attr)))
                        item)
                  attributes))
           ((symbolp item)
            (push item attr))
           (t
            (push item content))))
    (append (list name attributes) (reverse content))))

(defun preprocess (element)
  "Preprocess XML-style element.

XML elements should be lists of the following form:

  (name attrs &rest content)

name      Name of element as a string.
attrs     Element attributes as a plist of strings.
content   Element content, as strings or nested elements.

However, an alternative form is:

  (:name :attr value content)

`preprocess' will transform the alternative form into the regular form."
  (if (symbolp (first element))
      (preprocess-force element)
      element))

(defmacro with-element ((element name attrs content) &body body)
  "Wrap the body in a lambda for `call-with-element'."
  `(call-with-element ,element
                      (lambda (,name ,attrs ,content)
                        ,@body)))

(defun call-with-element (element function)
  "Call a function with the given element.

XML elements should be lists of the following form:

  (name attrs &rest content)

name      Name of element as a string.
attrs     Element attributes as a plist of strings.
content   Element content, as strings or nested elements.

However, an alternative form is:

  (:name :attr value content)

See also `preprocess'."
  (let* ((element (preprocess element))
         (name (first element))
         (attrs (second element))
         (content (cddr element)))
    (funcall function name attrs content)))
