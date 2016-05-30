(in-package :frelia-rendering)

(defclass macro-renderer ()
  ((macros-table :accessor macros-table
                 :initform (make-hash-table))))

(defmethod lookup-macro ((renderer macro-renderer) symbol)
  (gethash symbol (macros-table renderer)))

(defmethod register-macro ((renderer macro-renderer) symbol function)
  (setf (gethash symbol (macros-table renderer)) function))

(defmethod macrop ((renderer macro-renderer) element)
  (and (listp element)
       (lookup-macro renderer (first element))))

(defmethod get-macro-function ((renderer macro-renderer) element)
  (lookup-macro renderer (first element)))

(defmethod render-macros ((renderer macro-renderer) context element)
  (cond
    ((stringp element) element)
    ((symbolp element) element)
    ((macrop renderer element)
     (let* ((macro (get-macro-function element))
            (expanded-element (apply macro context (rest element))))
       (render-macros renderer context expanded-element)))
    ((listp element)
     (loop
       for subelement in element
       collect (render-macros renderer context subelement)))
    (t "")))
