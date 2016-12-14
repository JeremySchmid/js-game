(let ((chunk-size 256)
		(loaded-chunks-table (make-hash-table :test 'equal))
		(loaded-chunks-list nil))

  (defun get-rel-coord-and-chunk (abs-coord)
	 (floor abs-coord chunk-size))

  (defun get-chunk (abs-coord)
	 (get-rel-coord-and-chunk abs-coord))

  (defun get-rel-coord (abs-coord)
	 (mod abs-coord chunk-size))

  (defun get-chunk-coord (chunk-x)
	 (* chunk-x chunk-size))

  (defun tile-value-char (value)
	 (cond ((= value 0) "@")
			 ((= value 1) ".")
			 ((= value 2) "#")
			 (t "e")))

  (defun set-tile-value (row column value)
	 (setf (aref (get-loaded-area (get-chunk row) (get-chunk column))
					 (+ (* (get-rel-coord row) chunk-size) (get-rel-coord column)))
			 value))

  (defun get-tile-value (row column)
	 (aref (get-loaded-area (get-chunk row) (get-chunk column))
			 (+ (* (get-rel-coord row) chunk-size) (get-rel-coord column))))

  (defun set-tile-memory (row column value)
	 (setf (aref (get-loaded-area (get-chunk row) (get-chunk column))
					 (+ (* chunk-size chunk-size) (* (get-rel-coord row) chunk-size) (get-rel-coord column)))
			 value))

  (defun get-tile-memory (row column)
	 (aref (get-loaded-area (get-chunk row) (get-chunk column))
			 (+ (* chunk-size chunk-size) (* (get-rel-coord row) chunk-size) (get-rel-coord column))))

  (defun init-area (y x)
	 (let ((area (make-array (* x y 2) :element-type 'fixnum :initial-element 0)))
		area))

  (defun map-generation (y x)
	 (declare (ignore y x))
	 (if (< (random 400) 3)
		2
		1))


  (defun init-area-file (chunk-y chunk-x file-name)
	 (with-open-file (file file-name :direction :output :element-type 'fixnum)
		(let ((area-vector (init-area chunk-size chunk-size)))
		  (loop for rel-y from 0 to (- chunk-size 1)
				  do (loop for rel-x from 0 to (- chunk-size 1)
							  do (let* ((abs-y (+ (get-chunk-coord chunk-y) rel-y))
											(abs-x (+ (get-chunk-coord chunk-x) rel-x))
											(count (+ (* rel-y chunk-size) rel-x))
											(value (map-generation abs-y abs-x)))
									 (setf (aref area-vector count) value))))
		  (write-sequence area-vector file))))

  (defun area-filename (y x)
	 (let ((file-string (make-string-output-stream)))
		(format file-string "chunks/(~A,~A).map" y x)
		(get-output-stream-string file-string)))

  (defun load-area (y x)
	 (let ((file-name (area-filename y x))
			 (area-vector (make-array (* chunk-size chunk-size 2) :element-type 'fixnum))
			 (chunk (list y x)))
		(unless (probe-file file-name)
		  (init-area-file y x file-name))
		(with-open-file (file file-name :element-type 'fixnum)
		  (read-sequence area-vector file))
		(setf (gethash chunk loaded-chunks-table) area-vector)
		(push chunk loaded-chunks-list)
		(gethash chunk loaded-chunks-table)))

  (defun get-loaded-area (y x)
	 (let ((chunk (list y x)))
		(if (member chunk loaded-chunks-list :test 'equal)
		  (gethash chunk loaded-chunks-table)
		  (load-area y x))))

  )
