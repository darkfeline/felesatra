(in-package :frelia-site)

(defclass site-metadata ()
  ((url :initarg :url)))

(defmethod site-url ((instance site-metadata))
  (slot-value instance :url))

(defun make-site-metadata (&key url)
  (make-instance 'site-metadata :url url))
