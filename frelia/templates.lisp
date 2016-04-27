(in-package :templates)

(defmacro html-base-template (&key head-block title-block body-block)
  "HTML base template."
  `(string-join
    (doctype)
    (html `(("lang" . "en"))
          (head
           (meta `(("charset" . "UTF-8")))
           ,@head-block
           (title ,@title-block))
          (body ,@body-block))))

(defun base-template-header ()
  "Header for site base template."
  (header `(("id" . "site-header"))
          (h1 `(("id" . "site-title"))
              (site-a "/" "Feles Atra"))
          (ul `(("id" . "site-nav"))
              (li (site-a "about/" "About"))
              (li (site-a "projects/" "Projects"))
              (li (site-a "atom.xml"
                          (img `(("src" . ,(abs-url "img/site/feed-icon-14x14.png"))
                                 ("alt" . "Atom feed"))))))))

(defun base-template-footer ()
  "Footer for site base template."
  (footer `(("id" . "site-footer"))
          (address "The webmaster for this site is "
                   (pgp-key-a "darkfeline") ".")
          (div `(("id" . "site-copyright"))
               (p (span `(("xmlns:dct" . "https://purl.org/dc/terms/")
                          ("property" . "dct:title"))
                        "Feles Atra")
                  " by "
                  (a `(("xmlns:cc" . "https://creativecommons.org/ns#")
                       ("href" . "https://www.felesatra.moe")
                       ("property" . "cc:attributionName")
                       ("rel" . "cc:attributionURL"))
                     "Allen Li")
                  " is licensed under a "
                  (a `(("rel" . "license")
                       ("href" . "https://creativecommons.org/licenses/by-sa/4.0/"))
                     "Creative Commons Attribution-ShareAlike 4.0 International License")
                  ".")
               (a `(("rel" . "license")
                    ("href" . "https://creativecommons.org/licenses/by-sa/4.0/"))
                  (img `(("alt" . "Creative Commons License")
                         ("class" . "centered")
                         ("src" . "https://i.creativecommons.org/l/by-sa/4.0/88x31.png")))))))

(defmacro base-template (&key head-block title-block body-block)
  "Site base template."
  `(html-base-template
    :head-block ((meta `(("name" . "viewport")
                         ("content" . "width=device-width, initial-scale=1")))
                 (link `(("rel" . "stylesheet")
                         ("type" . "text/css")
                         ("href" . ,(abs-url "css/base.css"))))
                 (link `(("rel" . "stylesheet")
                         ("type" . "text/css")
                         ("href" . ,(abs-url "css/site.css"))))
                 (link `(("rel" . "icon")
                         ("type" . "image/png")
                         ("href" . ,(abs-url "img/site/favicon.png"))))
                 ,@head-block)
    :title-block ,title-block
    :body-block (,(base-template-header)
                 ,@body-block
                 ,(base-template-footer))))
