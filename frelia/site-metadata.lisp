(in-package :frelia-site)

(defclass rendering-context ()
  ((site-metadata :initarg :site-metadata)
   (resources :initarg :resources)
   current-resource))

(defclass site-metadata ()
  ((url :initarg :url)))

(defclass page-metadata ()
  ((path :initarg :path)
   (published :initarg :published)
   (modified :initarg :modified)
   (category :initarg :category)
   (tags :initarg :tags)))

(defgeneric site-url (instance)
  (:documentation
   "Get site URL.

The site URL is stored in `site-metadata', but this generic method works for
other classes that wrap `site-metadata'."))

(defmethod site-url ((instance site-metadata))
  (slot-value instance :url))

(defmethod site-url ((instance rendering-context))
  (site-url (slot-value instance :site-metadata)))
