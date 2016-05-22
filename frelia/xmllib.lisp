(in-package :frelia-xmllib)

(defun format-attrs (attrs)
  "Format attribute alist as \"key=value\" strings."
  (loop
    for (key . value) in attrs
    collect (format nil "~A=\"~A\"" key value)))

(defun render-content (render-function content)
  "Collect a list of strings, recursively rendering any sublists."
  (loop
    for item in content
    collect (cond
              ((listp item) (funcall render-function item))
              ((stringp item) item)
              (t ""))))

(defun lower-symbol-name (symbol)
  "Get lowercased name of SYMBOL."
  (string-downcase (symbol-name symbol)))

(defclass xml-element ()
  ((name :initarg :name :accessor element-name)
   (attrs :initarg :attrs :accessor element-attrs)
   (content :initarg :content :accessor element-content))
  (:default-initargs :name "undefined" :attrs '() :content '()))

(defun make-element (element)
  (cond
    ((symbolp (first element))
     (make-element-plist-attrs element))
    ((stringp (first element))
     (make-element-alist-attrs element))
    (t
     (error 'malformed-element-error :element element))))

(defun make-element-plist-attrs (element)
  "Make an element of the form:

  (:name :attr \"value\" \"content\")"
  (let ((builder (make-instance 'plist-element-builder)))
    (process-element builder element)
    (build builder)))

(defclass plist-element-builder ()
  (name
   attrs
   content
   pending-attr)
  (:documentation "Class for building elements from a plist attrs list."))

(defmethod process-element ((builder plist-element-builder) element)
  (let* ((name-symbol (first element))
         (name (lower-symbol-name name-symbol))
         (element-items (rest element)))
    (set-name builder name)
    (loop
      for item in element-items
      do (process-item builder item))))

(defmethod set-name ((builder plist-element-builder) name)
  (setf (slot-value builder 'name) name))

(defmethod process-item ((builder plist-element-builder) item)
  (cond
    ((has-pending-attr-p builder)
     (let* ((pending-attr ((pop-pending-attr builder)))
            (new-attr (cons pending-attr item)))
       (push-attr builder new-attr)))
    ((symbolp item)
     (let ((pending-attr (lower-symbol-name item)))
       (push-pending-attr builder pending-attr)))
    (t
     (push-content builder item))))

(defmethod has-pending-attr-p ((builder-plist-element-builder))
  (slot-value builder 'pending-attr))

(defmethod pop-pending-attr ((builder-plist-element-builder))
  (pop (slot-value builder 'pending-attr)))

(defmethod push-pending-attr ((builder-plist-element-builder) pending-attr)
  (push pending-attr (slot-value builder 'pending-attr)))

(defmethod push-content ((builder plist-element-builder) content)
  (push content (slot-value builder 'content)))

(defmethod push-attr ((builder plist-element-builder) attr)
  (push attr (slot-value builder 'attrs)))

(defmethod build ((builder plist-element-builder))
  (make-instance 'xml-element
                 :name name
                 :attrs (reverse attrs)
                 :content (reverse content)))

(defun make-element-alist-attrs (element)
  "Make an element of the form:

  (\"name\" ((\"key\" . \"value\")) \"content\")"
  (make-instance 'xml-element
                 :name (first element)
                 :attrs (second element)
                 :content (cddr element)))

(define-condition malformed-element-error (error)
  ((element :initarg :element)))

(defmacro with-element ((element name attrs content) &body body)
  `(funcall #'call-with-element ,element (lambda (,name ,attrs ,content) ,@body)))

(defun call-with-element (element function)
  (let* ((element (make-element element))
         (name (element-name element))
         (attrs (element-attrs element))
         (content (element-content element)))
    (funcall function name attrs content)))
