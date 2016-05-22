(in-package :frelia-site)

(defclass site-metadata ()
  ((url :initarg :url)))

(defgeneric site-url (instance)
  (:documentation "Get site URL."))

(defmethod site-url ((instance site-metadata))
  (slot-value instance :url))

(defclass file-resource ()
  ((path :initarg :path)
   (source :initarg :source :accessor file-source)))

(defclass page-resource ()
  ((metadata :initarg :metadata)
   (content :initarg :content :accessor page-content)))

(defclass page-metadata ()
  ((path :initarg :path)
   (content-page :initarg :content-page)
   (published :initarg :published)
   (modified :initarg :modified)
   (category :initarg :category)
   (tags :initarg :tags)))

(defgeneric resource-path (resource)
  (:documentation
   "Get the path of the resource."))

(defmethod resource-path ((resource file-resource))
  (slot-value resource :path))

(defmethod resource-path ((resource page-resource))
  (slot-value (slot-value resource :metadata) :path))

(defgeneric set-resource-path (resource path))

(defmethod set-resource-path ((resource file-resource) path)
  (setf (slot-value resource :path) path))

(defmethod set-resource-path ((resource page-resource) path)
  (setf (slot-value (slot-value resource :metadata) :path) path))

(defun load-page-resource (form)
  "Load page resource from a description form."
  (let ((args (loop
                for (key value) on form by #'cddr
                append (list key (eval value)))))
    (make-instance
     'page-resource
     :metadata (apply #'make-instance
                      'page-metadata
                      args)
     :content (getf args :content))))

(defun load-resources (root-path)
  "Load resources from a directory tree."
  (let* (site-resources
         (root-path (namestring (truename root-path)))
         (root-length (length root-path)))
    (flet ((add-resource (resource)
             (push resource site-resources))
           (target-path (pathname)
             (subseq (namestring pathname) root-length)))
      (cl-fad:walk-directory
       root-path
       (lambda (pathname)
         (cond
           ((string= (pathname-type pathname) "lisp")
            (let (resource)
              (with-open-file (file pathname)
                (setf resource (load-page-resource (read file))))
              (set-resource-path resource (target-path pathname))
              (add-resource resource)))
           (t
            (add-resource
             (make-instance
              'file-resource
              :path (target-path pathname)
              :source pathname)))))))
    site-resources))
