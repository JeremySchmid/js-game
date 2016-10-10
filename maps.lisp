(let ((chunk-size 256)
		(coord-size 1.0)
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

  (defun recanon-coord (float-x)
	 (multiple-value-bind (coord flot) (floor (+ float-x (/ coord-size 2.0)) coord-size)
		(setf flot (- flot (/ coord-size 2)))
		(values coord flot)))

  (defun recanon-loc (map-location)
	 (let ((return-loc map-location))
		(do ((is-x? 0 (1+ is-x?)))
		  ((> is-x? 1))
		  (multiple-value-bind (a b) (recanon-coord (aref return-loc (+ 2 is-x?)))
			 (setf (aref return-loc (+ 0 is-x?)) (+ a (aref return-loc (+ 0 is-x?))))
			 (setf (aref return-loc (+ 2 is-x?)) b)))
		return-loc))

  (defun tile-value-char (value)
	 (cond ((= value 0) "@")
			 ((= value 1) ".")
			 ((= value 2) "#")
			 (t "e")))

  (defun get-tile-value (row column)
	 (aref (get-loaded-area (get-chunk row) (get-chunk column))
			 (+ (* (get-rel-coord row) chunk-size) (get-rel-coord column))))

  (defun print-square (player-loc row column)
	 (princ (if (and (equal (aref player-loc 0) row)
						  (equal (aref player-loc 1) column))
				 (tile-value-char 0)
				 (tile-value-char (get-tile-value row column)))))

  (defun print-row (player-loc row x-borders)
	 (do ((column (aref x-borders 0) (1+ column))) 
		((> column (aref x-borders 1)))
		(print-square player-loc row column)))

  (defun print-area (player-loc y-borders x-borders)
	 (do ((row (aref y-borders 0) (1+ row)))
		((> row (aref y-borders 1)))
		(print-row player-loc row x-borders)
		(format t "~%")))

  (defun init-area (y x)
	 (let ((area (make-array (* x y) :element-type 'fixnum :initial-element 0)))
		area))

  (defun map-generation (y x)
	 (if (and (or (= (mod y 11) 0)
					  (= (mod x 11) 0))
				 (and (not (= (mod y 11) 5))
						(not (= (mod x 11) 5))))
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
		(format file-string *path-prefix*)
		(format file-string "chunks/(~A,~A).map" y x)
		(get-output-stream-string file-string)))

  (defun load-area (y x)
	 (let ((file-name (area-filename y x))
			 (area-vector (make-array (* chunk-size chunk-size) :element-type 'fixnum))
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
		  (load-area y x)))))
