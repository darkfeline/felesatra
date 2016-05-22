(in-package :frelia-site)

(defun load-resources (root-path)
  "Load resources from a directory tree."
  (let ((loader (make-resource-loader root-path)))
    (loader-load loader)
    (get-resources loader)))

(defclass resource-loader ()
  ((root-path :initarg :root-path)
   (targeter :initarg :targeter)
   resources))

(defun make-resource-loader (root-path)
  (let* ((targeter (make-path-targeter root-path)))
    (make-instance 'resource-loader
                   :root-path root-path
                   :targeter targeter)))

(defclass path-targeter ()
  (root-length :initarg :root-length))

(defun make-path-targeter (root-path)
  (make-instance 'path-targeter
                 :root-length (length (realpath root-path))))

(defun realpath (path)
  "Get the real absolute path of a path descriptor as a string."
  (namestring (truename path)))

(defmethod get-target-path ((targeter path-targeter) pathname)
  (subseq (realpath pathname) (slot-value targeter 'root-length)))

(defmethod get-target-path ((loader resource-loader) pathname)
  (get-target-path (slot-value loader 'targeter) pathname))

(defmethod loader-load ((loader resource-loader))
  (cl-fad:walk-directory
   (slot-value loader 'root-path)
   (lambda (pathname)
     (load-resource-from-pathname loader pathname))))

(defmethod load-resource-from-pathname ((loader resource-loader) pathname)
  (let ((resource-maker (make-resource-maker loader pathname)))
    (add-resource (make resource-maker))))

(defun make-resource-maker ((loader resource-loader) pathname)
  (cond
    ((lisp-file-p pathname)
     (make-page-resource-maker loader pathname)
     (t
      (make-file-resource-maker loader pathname)))))

(defun lisp-file-p (pathname)
  (string= (pathname-type pathname) "lisp"))

(defclass file-resource-maker ()
  ((pathname :initarg :pathname)
   (loader :initarg :loader)))

(defun make-file-resource-maker (loader pathname)
  (make-instance 'file-resource-maker
                 :pathname pathname
                 :loader loader))

(defmethod make ((maker file-resource-maker))
  (make-instance
   'file-resource
   :path (get-target-path maker pathname)
   :source (slot-value maker 'pathname)))

(defmethod get-target-path ((maker file-resource-maker) pathname)
  (get-target-path (slot-value maker 'loader) pathname))

(defclass page-resource-maker ()
  ((pathname :initarg :pathname)
   (loader :initarg :loader)))

(defun make-page-resource-maker (loader pathname)
  (make-instance 'page-resource-maker
                 :pathname pathname
                 :loader loader))

(defmethod make ((maker page-resource-maker))
  (let (resource
        (pathname (slot-value maker 'pathname)))
    (with-open-file (file pathname)
      (setf resource (make-page-resource-from-plist (read file))))
    (setf (resource-path resource) (get-target-path maker pathname))))

(defmethod get-target-path ((maker page-resource-maker) pathname)
  (get-target-path (slot-value maker 'loader) pathname))

(defun make-page-metadata-from-plist (metadata-plist)
  (apply #'make-instance 'page-metadata metadata-plist))

(defun make-page-resource-from-plist (resource-plist)
  "Load page resource from a description form."
  (let ((resource-plist (eval-plist-values resource-plist)))
    (make-instance
     'page-resource
     :metadata (make-page-metadata-from-plist resource-plist)
     :content (getf resource-plist :content))))

(defun eval-plist-values (plist)
  "Evaluate the values in a plist."
  (loop
    for (key value) on plist by #'cddr
    append (list key (eval value))))

(defmethod add-resource ((loader resource-loader) resource)
  (push resource (slot-value loader 'resources)))

(defmethod get-resources ((loader resource-loader))
  (slot-value loader 'resources))
