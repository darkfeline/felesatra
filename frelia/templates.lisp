(in-package :frelia-templates)

(defun html-base-template (&key title head-block body-block)
  "HTML base template."
  `(:html :lang "en"
          (:head
           (:meta :charset "UTF-8")
           (:title ,title)
           ,@head-block)
          (:body ,@body-block)))

(defvar *jquery-script*
  `(:script :src "https://code.jquery.com/jquery-2.2.4.min.js"))

(defvar *ref-script*
  `(:script ,(alexandria:read-file-into-string "frelia-resources/ref-script.js")))

(defvar *base-head-block*
  '((:meta :name "viewport"
           :content "width=device-width, initial-scale=1")
    (:link :rel "stylesheet"
           :type "text/css"
           :href (:abs-url "css/base.css"))
    (:link :rel "stylesheet"
           :type "text/css"
           :href (:abs-url "css/site.css"))
    (:link :rel "icon"
           :type "image/png"
           :href (:abs-url "img/site/favicon.png"))))

(defvar *base-header*
  `(:header :id "site-header"
            (:h1 :id "site-title"
                 (:site-a "/" "Feles Atra"))
            (:ul :id "site-nav"
                 (:li (:site-a "about/" "About"))
                 (:li (:site-a "projects/" "Projects"))
                 (:li (:site-a "atom.xml"
                               (:img :src (:abs-url "img/site/feed-icon-14x14.png")
                                     :alt "Atom feed"))))))

(defvar *base-footer*
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

(defun base-template (&key title head-block body-block)
  "Site base template."
  (html-base-template
   :title title
   :head-block `(,@*base-head-block*
                 ,@head-block)
   :body-block `(,*base-header*
                 ,@body-block
                 ,*base-footer*)))

(defun content-page-template (&key title head-block content-block)
  "Content page template."
  (base-template
   :title title
   :head-block head-block
   :body-block
   `((:section
      (:header
       :class "content-header"
       (:h1 ,title)
       (:eval-with-context
        (context)
        (let* ((metadata (gethash :metadata context)))
          `(:dl
            ,@(let ((published (gethash :published metadata)))
                (when published
                  `((:dt "Published")
                    (:dd ,published))))
            ,@(let ((updated (gethash :updated metadata)))
                (when updated
                  `((:dt "Updated")
                    (:dd ,updated))))
            ,@(let ((category (gethash :category metadata)))
                (when category
                  `((:dt "Category")
                    (:dd ,category))))
            ,@(let ((tags (gethash :tags metadata)))
                (when tags
                  `((:dt "Tags")
                    ,@(loop for tag in tags
                            collect `(:dd ,tag)))))))))
      ,@content-block))))
