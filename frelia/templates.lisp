(in-package :frelia-templates)

(defclass html-base-template ()
  ((title :initarg :title
          :accessor title)
   (head-block :initarg :head-block
               :accessor head-block)
   (body-block :initarg :body-block
               :accessor body-block))
  (:documentation "HTML base template."))

(defgeneric render (template))

(defmethod head-block (template))

(defmethod body-block (template))

(defmethod render ((template html-base-template))
  (let ((title (title template))
        (head-block (head-block template))
        (body-block (body-block template)))
    `(:html :lang "en"
            (:head
             (:meta :charset "UTF-8")
             (:title ,title)
             ,@head-block)
            (:body ,@body-block))))

(defclass base-template (html-base-template)
  (body-block :initarg :content-block :accessor content-block)
  (:documentation "Site base template."))

(defgeneric include-jquery-p (template))

(defmethod include-jquery-p ((template base-template))
  nil)

(defgeneric include-ref-script-p (template)
  (:documentation "Whether to include ref/footnote Javascript."))

(defmethod include-ref-script-p ((template base-template))
  nil)

(defvar jquery-script
  `(:script :src "https://code.jquery.com/jquery-2.2.4.min.js"))

(defvar ref-script
  `(:script ,(alexandria:read-file-into-string "frelia-resources/ref-script.js")))

(defmethod head-block ((template base-template))
  `((:meta :name "viewport"
           :content "width=device-width, initial-scale=1")
    (:link :rel "stylesheet"
           :type "text/css"
           :href (:abs-url "css/base.css"))
    (:link :rel "stylesheet"
           :type "text/css"
           :href (:abs-url "css/site.css"))
    (:link :rel "icon"
           :type "image/png"
           :href (:abs-url "img/site/favicon.png"))
    ,@(if (include-jquery-p template) (list jquery-script) '())
    ,@(if (include-ref-script-p template) (list ref-script) '())
    ,(call-next-method)))

(defmethod body-block ((template base-template))
  (list (base-header)
        (content-block template)
        (base-footer)))

(defun base-header ()
  "Header for site base template."
  `(:header :id "site-header"
            (:h1 :id "site-title"
                 (:site-a "/" "Feles Atra"))
            (:ul :id "site-nav"
                 (:li (:site-a "about/" "About"))
                 (:li (:site-a "projects/" "Projects"))
                 (:li (:site-a "atom.xml"
                               (:img :src (:abs-url "img/site/feed-icon-14x14.png")
                                     :alt "Atom feed"))))))

(defun base-footer ()
  "Footer for site base template."
  `(:footer
    :id "site-footer"
    (:address "The webmaster for this site is "
              (:pgp-key-a "darkfeline") ".")
    (:div :id "site-copyright"
          (:p "Feles Atra is licensed under a "
              (:a :rel "license"
                  :href "https://creativecommons.org/licenses/by-sa/4.0/"
                  "Creative Commons Attribution-ShareAlike 4.0 International License")
              ".")
          (:a :rel "license"
              :href "https://creativecommons.org/licenses/by-sa/4.0/"
              (:img :alt "Creative Commons License"
                    :class "centered"
                    :src "https://i.creativecommons.org/l/by-sa/4.0/88x31.png")))))

(defclass content-page-template (base-template)
  ()
  (:documentation "Content page template."))

(defmethod content-block ((template content-page-template))
  `((:section
     (:header
      :class "content-header"
      (:h1 ,title)
      (:eval-with-data
       (data)
       (let* ((page-resource (current-resource data))
              (metadata (metadata page-resource)))
         `(:dl
           ,@(let ((published (published-date metadata)))
               (when published
                 `((:dt "Published")
                   (:dd ,published))))
           ,@(let ((modified (modified-date metadata)))
               (when modified
                 `((:dt "Modified")
                   (:dd ,modified))))
           ,@(let ((category (page-category metadata)))
               (when category
                 `((:dt "Category")
                   (:dd ,category))))
           ,@(let ((tags (page-tags metadata)))
               (when tags
                 `((:dt "Tags")
                   ,@(loop for tag in tags
                           collect `(:dd ,tag)))))))))
     ,@(call-next-method))))
