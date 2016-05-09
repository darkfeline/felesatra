(in-package "FRELIA-XMLLIB")

(defun collect-attrs (attributes)
  (loop
    for (key . value) in attributes
    collect (format nil "~A=\"~A\"" key value)))

(defun collect-content (render-function content)
  (loop
    for item in content
    collect (cond
              ((listp item) (funcall render-function item))
              ((stringp item) item)
              (t ""))))
