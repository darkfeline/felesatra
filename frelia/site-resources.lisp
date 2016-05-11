(in-package :frelia-site)

(defclass file-resource ()
  ((path :initarg :path)
   (source :initarg :source)))

(defclass page-resource ()
  ((metadata :initarg :metadata)
   (content :initarg :content)))

(defun make-page-resource (path page-data)
  "Make page resource from page definition plist."
  (make-instance
   'page-resource
   :metadata (make-instance
              'page-metadata
              :path path
              :published (getf page-data :published)
              :modified (getf page-data :modified)
              :category (getf page-data :category)
              :tags (getf page-data :tags))
   :content (getf page-data :content)))

(defgeneric resource-path (resource)
  (:documentation
   "Get the path of the resource.

For some resources this path is stored in a slot, but for others this may need
to be calculated or fetched in another manner."))

(defmethod resource-path ((resource file-resource))
  (slot-value resource :path))

(defmethod resource-path ((resource page-resource))
  (slot-value (slot-value resource :metadata) :path))

(defgeneric render (context resource target-dir)
  (:documentation "Render a resource."))

(defmethod render (context (resource file-resource) target-dir))

(defmethod render (context (resource page-resource) target-dir))

(defun load-resources (root-path)
  "Load resources from a directory tree."
  (let (site-resources
        (root-length (length (namestring (truename root-path)))))
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
                (setf resource (read file)))
              (add-resource (make-page-resource
                             (target-path pathname)
                             resource))))
           (t
            (add-resource
             (make-instance
              'file-resource
              :path (target-path pathname)
              :source pathname)))))))
    site-resources))

(defun render-resources (context resources target-dir)
  "Render resources."
  (loop
    for resource in resources
    do
       (setf (slot-value context :current-resource) resource)
       (render context resource target-dir)))
