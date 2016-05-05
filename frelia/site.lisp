(in-package "FRELIA-SITE")

(defclass site-metadata ()
  (url))

(defclass page-metadata ()
  (path
   content
   published
   modified
   category
   tags))

(defclass rendering-data ()
  (site-metadata
   page-index
   page-metadata))

(defgeneric site-url (instance))

(defmethod site-url ((instance site-metadata))
  (slot-value instance :url))

(defmethod site-url ((instance rendering-data))
  (site-url (slot-value instance :site-metadata)))

(defclass dir-resource ()
  (path)
  (:documentation "`path' should end in a slash."))

(defclass file-resource ()
  (path
   source))

(defclass page-resource ()
  (metadata
   content))

(defgeneric resource-path (resource))

(defmethod resource-path ((resource dir-resource))
  (slot-value resource :path))

(defmethod resource-path ((resource file-resource))
  (slot-value resource :path))

(defmethod resource-path ((resource page-resource))
  (slot-value (slot-value resource :metadata) :path))

(defgeneric render (resource rendering-data)
  (:documentation "Render a resource."))

(defmethod render ((resource dir-resource)
                   rendering-data)
  (ensure-directories-exist (string-join (site-url rendering-data)
                                         (resource-path resource))))

(defmethod render ((resource file-resource)
                   rendering-data))

(defmethod render ((resource page-resource)
                   rendering-data))

(defun build-site (site))
