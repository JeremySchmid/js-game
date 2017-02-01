
(defun hash-keys (table)
  (loop for key being the hash-keys of table collect key))

(defun hash-values (table)
  (loop for key in (hash-keys table)
		  collect (gethash key table)))

(defmacro toggle (var)
  `(setf ,var (not ,var)))

