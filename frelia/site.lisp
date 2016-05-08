(in-package "FRELIA-SITE")

(defclass site-metadata ()
  (url))

(defclass page-metadata ()
  ((path :initarg :path)
   published
   modified
   category
   tags))

(defclass rendering-data ()
  (site-metadata
   page-index
   page-metadata))

(defgeneric site-url (instance)
  (:documentation
   "Get site URL.

The site URL is stored in `site-metadata', but this generic method works for
other classes that wrap `site-metadata'."))

(defmethod site-url ((instance site-metadata))
  (slot-value instance :url))

(defmethod site-url ((instance rendering-data))
  (site-url (slot-value instance :site-metadata)))

(defclass file-resource ()
  ((path :initarg :path)
   (source :initarg :source)))

(defclass page-resource ()
  ((metadata :initarg :metadata)
   content))

(defgeneric resource-path (resource)
  (:documentation
   "Get the path of the resource.

For some resources this path is stored in a slot, but for others this may need
to be calculated or fetched in another manner."))

(defmethod resource-path ((resource file-resource))
  (slot-value resource :path))

(defmethod resource-path ((resource page-resource))
  (slot-value (slot-value resource :metadata) :path))

(defgeneric render (rendering-data resource)
  (:documentation "Render a resource."))

(defmethod render (rendering-data (resource file-resource)))

(defmethod render (rendering-data (resource page-resource)))

(defun load-resources (root-path)
  "Load resources from a directory tree."
  (let (site-resources
        (root-length (length (namestring (truename root-path)))))
    (flet ((add-resource (resource)
             (setf site-resources
                   (cons resource site-resources)))
           (target-path (pathname)
             (subseq (namestring pathname) root-length)))
      (cl-fad:walk-directory
       root-path
       (lambda (pathname)
         (cond
           ((string= (pathname-type pathname) "lisp")
            (add-resource
             (make-instance
              'page-resource
              :metadata (make-instance
                         'page-metadata
                         :path (target-path pathname)))))
           (t
            (add-resource
             (make-instance
              'file-resource
              :path (target-path pathname)
              :source pathname)))))))
    site-resources))
