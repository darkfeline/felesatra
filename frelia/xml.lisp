(in-package :xml)

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
  "Format XML attributes from plist."
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

(defmacro with-keys (vars args &rest form)
  "Eval form with keys and tokens from args."
  (destructuring-bind (keys tokens) vars
    `(let* ((,tokens (parse-keys ,args))
            (,keys (pop ,tokens)))
       ,@form)))

(defun make-tag (name &rest args)
  "Make HTML tag with given name and passing given tokens.

`(make-tag \"html\" :lang \"en\" \"content\")'"
  (with-keys (keys tokens) args
    (string-join
     (list "<" name (format-attr keys) ">"
           (string-join tokens) "</" name ">"))))

(defun make-empty-tag (name &rest args)
  "Make empty (self-closing) XML tag with given name and passing given tokens."
  (with-keys (keys tokens) args
    (string-join
     (list "<" name (format-attr keys) "/>"))))

(defun make-decl-tag (name &rest args)
  "Make declaration XML tag with given name and passing given tokens."
  (with-keys (keys tokens) args
    (string-join
     (list "<" name (format-attr keys) "/>"))))

(defmacro deftag (name &optional type)
  `(defun ,name (&rest tokens)
     (apply (quote ,(cond
                      ((eq type :empty) 'make-empty-tag)
                      ((eq type :decl) 'make-decl-tag)
                      (t 'make-tag)))
            ,(string-downcase (symbol-name name)) tokens)))
