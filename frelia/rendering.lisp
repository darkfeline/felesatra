(in-package :frelia-rendering)

(defclass macro-renderer ()
  ((macros-table :accessor macros-table
                 :initform (make-hash-table))))

(defmethod lookup-macro ((renderer macro-renderer) symbol)
  (gethash symbol (macros-table renderer)))

(defmethod register-macro ((renderer macro-renderer) symbol func)
  (setf (gethash symbol (macros-table renderer)) func))

(defmethod macrop ((renderer macro-renderer) form)
  (and (listp form)
       (lookup-macro renderer (first form))))

(defmethod get-macro-function ((renderer macro-renderer) form)
  (lookup-macro renderer (first form)))

(defmethod render-macros ((renderer macro-renderer) context form)
  (cond
    ((stringp form) form)
    ((symbolp form) form)
    ((macrop renderer form)
     (let* ((macro (get-macro-function renderer form))
            (expanded-form (apply macro context (rest form))))
       (render-macros renderer context expanded-form)))
    ((listp form)
     (loop
       for subelement in form
       collect (render-macros renderer context subelement)))
    (t "")))
