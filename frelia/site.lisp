(in-package "FRELIA-SITE")

(defstruct site-metadata
  url)

(defstruct page-metadata
  "Page metadata."
  path
  content
  published
  modified
  category
  tags)

(defstruct rendering-data
  site-metadata
  page-index
  page-metadata)

(defun get-site-url (rendering-data))

(defgeneric render (resource rendering-data)
  (:documentation "Render a resource."))

(defclass dir-resource ()
  (path :accessor dir-resource-path))

(defmethod render ((resource dir-resource)
                   rendering-data)
  (ensure-directories-exist (string-join (get-site-url rendering-data)
                                         (dir-resource-path resource))))

(defclass file-resource ()
  (path
   source))

(defmethod render ((resource file-resource)
                   rendering-data))

(defclass page-resource ()
  (metadata
   content))

(defmethod render ((resource page-resource)
                   rendering-data))

(defun build-site (site))
