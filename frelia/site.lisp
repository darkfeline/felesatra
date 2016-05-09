(in-package "FRELIA-SITE")

(defclass rendering-data ()
  (site-metadata
   page-index
   page-metadata))

(defclass site-metadata ()
  (url))

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

(defmethod site-url ((instance rendering-data))
  (site-url (slot-value instance :site-metadata)))

(defclass file-resource ()
  ((path :initarg :path)
   (source :initarg :source)))

(defclass page-resource ()
  ((metadata :initarg :metadata)
   (content :initarg :content)))

(defun make-page-resource (path page-data)
  "Make page resource from page definition plist."
  (make-instance
   'page-resource
   :metadata (make-instance
              'page-metadata
              :path path
              :published (getf page-data :published)
              :modified (getf page-data :modified)
              :category (getf page-data :category)
              :tags (getf page-data :tags))
   :content (getf page-data :content)))

(defgeneric resource-path (resource)
  (:documentation
   "Get the path of the resource.

For some resources this path is stored in a slot, but for others this may need
to be calculated or fetched in another manner."))

(defmethod resource-path ((resource file-resource))
  (slot-value resource :path))

(defmethod resource-path ((resource page-resource))
  (slot-value (slot-value resource :metadata) :path))

(defgeneric render (rendering-data resource)
  (:documentation "Render a resource."))

(defmethod render (rendering-data (resource file-resource)))

(defmethod render (rendering-data (resource page-resource)))

(defun load-resources (root-path)
  "Load resources from a directory tree."
  (let (site-resources
        (root-length (length (namestring (truename root-path)))))
    (flet ((add-resource (resource)
             (setf site-resources
                   (cons resource site-resources)))
           (target-path (pathname)
             (subseq (namestring pathname) root-length)))
      (cl-fad:walk-directory
       root-path
       (lambda (pathname)
         (cond
           ((string= (pathname-type pathname) "lisp")
            (let (resource)
              (with-open-file (file pathname)
                (setf resource (read file)))
              (add-resource (make-page-resource
                             (target-path pathname)
                             resource))))
           (t
            (add-resource
             (make-instance
              'file-resource
              :path (target-path pathname)
              :source pathname)))))))
    site-resources))

(defvar *site-macros* (make-hash-table))

(defun make-keyword (symbol)
  (intern (symbol-name symbol) "KEYWORD"))

(defmacro defsitemacro (name lambda-list &body body)
  `(progn
     (defun ,name ,lambda-list
       ,@body)
     (setf (gethash ,(make-keyword name) *site-macros*)
           (quote name))))

(defun prerender-html (rendering-data element)
  "Prerender HTML by expanding site-specific macros."
  (render-macros rendering-data element))

(defun render-macros (rendering-data element)
  "Expand site-specific macros."
  (cond
    ((stringp element) element)
    ((symbolp element) element)
    ((and (listp element)
          (gethash (first element) *site-macros*))
     (let ((macro (gethash (first element) *site-macros*)))
       (render-macros rendering-data
                      (apply macro rendering-data (rest element)))))
    ((listp element)
     (loop
       for subelement in element
       collect (render-macros rendering-data subelement)))
    (t "")))

(defsitemacro abs-url (rendering-data path)
  "Get absolute URL for site path."
  (concatenate 'string (site-url rendering-data) (string-left-trim "/" path)))

(defsitemacro pgp-key-a (rendering-data &rest content)
  "<a> tag to my PGP key."
  `(:a :href "https://sks-keyservers.net/pks/lookup?op=get&search=0x871AC6C82D45F74D"
       ,@content))

(defsitemacro site-a (rendering-data path &rest content)
  "<a> tag to site path."
  `(:a :href (:abs-url ,path) ,@content))
