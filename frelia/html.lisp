(in-package :html)

(defun parse-keys (args)
  "Parse keyword arguments.

Returns `(keys-plist . single-args)'"
  (let (keys tokens current-key)
    (loop
      for arg in args
      do (cond
           (current-key
            (setf (getf keys (pop current-key)) arg))
           ((keywordp arg) (push arg current-key))
           (t (push arg tokens))))
    (cons keys (reverse tokens))))

(defun format-attr (attrs)
  "Format HTML attributes from plist."
  (let (strings current-attr)
    (loop
      for arg in attrs
      do (cond
           (current-attr
            (push (format nil " ~A=\"~A\""
                          (string-downcase (symbol-name (pop current-attr)))
                          arg)
                  strings))
           (t (push arg current-attr))))
    (string-join strings)))

(defun make-tag (name &rest tokens)
  "Make HTML tag with given name and passing given tokens.

`(make-tag \"html\" :lang \"en\" \"content\")'"
  (let* ((tokens (parse-keys tokens))
         (keys (pop tokens)))
    (string-join
     (list "<" name (format-attr keys) ">"
           (string-join tokens) "</" name ">"))))

(defun make-empty-tag (name &rest tokens)
  "Make empty (self-closing) HTML tag with given name and passing given tokens."
  (let* ((tokens (parse-keys tokens))
         (keys (pop tokens)))
    (string-join
     (list "<" name (format-attr keys) "/>"))))

(defmacro deftag (name &key empty)
  `(defun ,name (&rest tokens)
     (apply (if ,empty 'make-empty-tag 'make-tag)
            ,(string-downcase (symbol-name name)) tokens)))
