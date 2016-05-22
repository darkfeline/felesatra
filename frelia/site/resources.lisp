(in-package :frelia-site)

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

(defmethod resource-path ((resource file-resource))
  (slot-value resource :path))

(defmethod resource-path ((resource page-resource))
  (slot-value (slot-value resource :metadata) :path))

(defmethod set-resource-path ((resource file-resource) path)
  (setf (slot-value resource :path) path))

(defmethod set-resource-path ((resource page-resource) path)
  (setf (slot-value (slot-value resource :metadata) :path) path))

(defsetf resource-path set-resource-path)
