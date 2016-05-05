(in-package "FRELIA-SITE-MACROS")

(defvar site-url "https://www.felesatra.moe")

(defun abs-url (path)
  "Get absolute URL for site path."
  (string-join site-url "/" (string-left-trim "/" path)))

(defun pgp-key-a (&rest content)
  "`a' tag to my PGP key."
  (a
   `(("href" . "https://sks-keyservers.net/pks/lookup?op=get&search=0x871AC6C82D45F74D"))
   content))

(defun site-a (path &rest content)
  "`a' tag to site path."
  (a `(("href" . ,(abs-url path))) content))
