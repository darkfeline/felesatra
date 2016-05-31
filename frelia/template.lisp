(in-package :frelia-template)

(defclass template-renderer ()
  ((templates :initform (make-hash-table)
              :accessor templates)))

(defmethod register-template ((renderer template-renderer) symbol function)
  (setf (gethash symbol (templates renderer)) function))

(defmethod render-template ((renderer template-renderer) template-symbol context)
  (apply (gethash template-symbol (templates renderer)) :allow-other-keys t context))
