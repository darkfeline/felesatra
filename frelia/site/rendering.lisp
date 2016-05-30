(in-package :frelia-rendering)

(defclass rendering-context ()
  ((site-metadata :initarg :site-metadata)
   (resources :initarg :resources)
   (current-resource :accessor current-resource))
  (:documentation "Context passed to every rendering function."))

(defmethod site-url ((instance rendering-context))
  (site-url (slot-value instance :site-metadata)))

(defgeneric render (resource context target-dir)
  (:documentation "Render a resource."))

(defmethod render ((resource file-resource) context target-dir)
  (sb-posix:link (file-source resource)
                 (concatenate 'string target-dir (resource-path resource))))

(defmethod render ((resource page-resource) context target-dir)
  (let* ((content (page-content resource))
         (content (render-macros content))
         (html (frelia-html:render-html content)))))

(defun render-resources (context resources target-dir)
  "Render resources."
  (loop
    for resource in resources
    do
       (setf (current-resource context) resource)
       (render resource context target-dir)))
