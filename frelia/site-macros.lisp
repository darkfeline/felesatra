(in-package "FRELIA-SITE-MACROS")

(defun abs-url (site-metadata path)
  "Get absolute URL for site path."
  (string-join (site-url site-metadata) "/" (string-left-trim "/" path)))

(defun pgp-key-a (&rest content)
  "`a' tag to my PGP key."
  (a
   `(("href" . "https://sks-keyservers.net/pks/lookup?op=get&search=0x871AC6C82D45F74D"))
   content))

(defun site-a (site-metadata path &rest content)
  "`a' tag to site path."
  (a `(("href" . ,(abs-url site-metadata path))) content))
