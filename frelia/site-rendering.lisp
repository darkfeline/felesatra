(in-package :frelia-site)

(defclass rendering-context ()
  ((site-metadata :initarg :site-metadata)
   (resources :initarg :resources)
   current-resource)
  (:documentation "Context passed to every rendering function."))

(defmethod site-url ((instance rendering-context))
  (site-url (slot-value instance :site-metadata)))

(defgeneric render (context resource target-dir)
  (:documentation "Render a resource."))

(defmethod render (context (resource file-resource) target-dir)
  (sb-posix:link (resource-path resource) target-dir))

(defmethod render (context (resource page-resource) target-dir))

(defun render-resources (context resources target-dir)
  "Render resources."
  (loop
    for resource in resources
    do
       (setf (slot-value context :current-resource) resource)
       (render context resource target-dir)))
