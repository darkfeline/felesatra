(in-package :frelia-resource)

(defclass resource-loader ()
  ((root-path :initarg :root-path :accessor root-path)
   (resources :initform () :accessor resources)))

(defmethod initialize-instance :after ((instance resource-loader) &rest initargs)
  (with-accessors ((root-path root-path)) instance
    (setf root-path (realpath root-path))))

(defun realpath (path)
  "Get the real absolute path of a path descriptor as a string."
  (namestring (truename path)))

(defmethod get-target-path ((loader resource-loader) pathname)
  (subseq (realpath pathname) (length (root-path loader))))

(defmethod load-resources ((loader resource-loader))
  (cl-fad:walk-directory
   (root-path loader)
   (lambda (pathname)
     (load-resource-from-pathname loader pathname))))

(defmethod add-resource ((loader resource-loader) resource)
  (push resource (resources loader)))

(defmethod load-resource-from-pathname ((loader resource-loader) source)
  (let ((path (get-target-path loader source)))
    (add-resource loader
                  (cond
                    ((lisp-file-p source)
                     (make-page-resource path source))
                    (t
                     (make-instance 'file-resource
                                    :source source
                                    :path path))))))

(defun lisp-file-p (pathname)
  (string= (pathname-type pathname) "lisp"))

(defun make-page-resource (path source)
  (with-open-file (file source)
    (make-instance 'page-resource
                   :path path
                   :context (eval (read file)))))

(defclass base-resource ()
  ((path :initarg :path :accessor resource-path)))

(defclass file-resource (base-resource)
  ((source :initarg :source :accessor file-source)))

(defclass page-resource (base-resource)
  ((context :initarg :context :accessor page-context)))

(defmethod print-object ((object file-resource) stream)
  (format stream "#<FILE-RESOURCE :path ~S :source ~S>"
          (resource-path object)
          (file-source object)))

(defmethod print-object ((object page-resource) stream)
  (format stream "#<PAGE-RESOURCE :path ~S :context ~S>"
          (resource-path object)
          (page-context object)))

(defclass resource-renderer ()
  ((context :initarg :context
            :accessor context)
   (macro-renderer :initarg :macro-renderer
                   :accessor macro-renderer)))

(defun add-plist-to-hash (plist table)
  (loop
    for (key value) on plist by #'cddr
    do (setf (gethash key table) value))
  table)

(defmethod render-resources ((renderer resource-renderer) target-dir))
