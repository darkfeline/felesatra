(in-package :frelia-xmllib)

(defun collect-attrs (attrs)
  "Format attribute alist as \"key=value\" strings."
  (loop
    for (key . value) in attrs
    collect (format nil "~A=\"~A\"" key value)))

(defun collect-content (render-function content)
  "Collect a list of strings, recursively rendering any sublists."
  (loop
    for item in content
    collect (cond
              ((listp item) (funcall render-function item))
              ((stringp item) item)
              (t ""))))

(defclass xml-element ()
  ((name :initarg :name :accessor element-name)
   (attrs :initarg :attrs :accessor element-attrs)
   (content :initarg :content :accessor element-content))
  (:default-initargs :name "undefined" :attrs '() :content '()))

(defun make-element-plist-attrs (element)
  "Make an element of the form:

  (:name :attr \"value\" \"content\")"
  (let ((name (string-downcase (symbol-name (first element))))
        attrs
        content)
    (loop
      with attr = nil
      for item in (rest element)
      do
         (cond
           (attr
            (push (cons (string-downcase (symbol-name (pop attr)))
                        item)
                  attrs))
           ((symbolp item)
            (push item attr))
           (t
            (push item content))))
    (make-instance 'xml-element
                   :name name
                   :attrs attrs
                   :content (reverse content))))

(defun make-element-alist-attrs (element)
  "Make an element of the form:

  (\"name\" ((\"key\" . \"value\")) \"content\")"
  (make-instance 'xml-element
                 :name (first element)
                 :attrs (second element)
                 :content (cddr element)))

(define-condition malformed-element-error (error)
  ((element :initarg :element)))

(defun make-element (element)
  (cond
    ((symbolp (first element))
     (make-element-plist-attrs element))
    ((stringp (first element))
     (make-element-alist-attrs element))
    (t
     (error 'malformed-element-error :element element))))

(defmacro with-element ((element name attrs content) &body body)
  `(funcall #'call-with-element ,element (lambda (,name ,attrs ,content) ,@body)))

(defun call-with-element (element function)
  (let* ((element (make-element element))
         (name (element-name element))
         (attrs (element-attrs element))
         (content (element-content element)))
    (funcall function name attrs content)))
