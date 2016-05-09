(in-package "FRELIA-TEMPLATES")

(defun html-base-template (&key title head-block body-block)
  "HTML base template."
  `(:html :lang "en"
          (:head
           (:meta :charset "UTF-8")
           (:title ,title)
           ,@head-block)
          (:body ,@body-block)))

(defun base-template-header ()
  "Header for site base template."
  (:header :id "site-header"
           (:h1 :id "site-title"
                (:site-a "/" "Feles Atra"))
           (:ul :id "site-nav"
                (:li (:site-a "about/" "About"))
                (:li (:site-a "projects/" "Projects"))
                (:li (:site-a "atom.xml"
                              (:img :src (:abs-url "img/site/feed-icon-14x14.png")
                                    :alt "Atom feed"))))))

(defun base-template-footer ()
  "Footer for site base template."
  (:footer
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
   :head-block
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
     ,@head-block)
   :body-block
   `(,@(base-template-header)
     ,@block-block
     ,@(base-template-footer))))

(defun content-page-template (&key title metadata head-block content-block)
  "Content page template."
  (base-template
   :title title
   :head-block head-block
   :body-block
   `(:section
     (:header
      :class "content-header"
      (:h1 ,title)
      (:dl
       ,@(let ((published (slot-value metadata :published)))
          (when published
            `((:dt "Published")
              (:dd ,published))))
       ,@(let ((modified (slot-value metadata :modified)))
           (when modified
             `((:dt "Modified")
               (:dd ,modified))))
       ,@(let ((category (slot-value metadata :category)))
           (when category
             `((:dt "Category")
               (:dd ,category)))))
      ,@(let ((tags (slot-value metadata :tags)))
          (when tags
            `((:dl
               (:dt "Tags")
               ,@(loop for tag in tags
                       collect `(:dd ,tag)))))))
     ,@content-block)))
