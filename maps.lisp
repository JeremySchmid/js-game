
(defparameter *loaded-areas* (make-array '(3 3)))

(defparameter *area-size* 8)

(defparameter *location* (vector 0 0 25 74 0.0 0.0))

(defun recanonicalize (location)
  (let* ((new-location location))
	 (loop for is-y? from 0 to 1
			 do (macrolet ((get-loc (n) `(aref new-location (+ is-y? ,n))))
					(let ((mod-num (floor (+ (get-loc 4) 0.5) 1.0)))
					  (when (> (get-loc 4) 0.5)
						 (setf (get-loc 4) (- (get-loc 4) mod-num))
						 (setf (get-loc 2) (+ (get-loc 2) mod-num)))
					  (when (<= (get-loc 4) -0.5)
						 (setf (get-loc 4) (- (get-loc 4) mod-num))
						 (setf (get-loc 2) (+ (get-loc 2) mod-num))))
					(let ((mod-num (floor (get-loc 2) 256)))
					  (when (> (get-loc 2) 256)
						 (setf (get-loc 2) (- (get-loc 2) (* 256 mod-num)))
						 (setf (get-loc 0) (+ (get-loc 0) mod-num)))
					  (when (<= (get-loc 2) 0)
						 (setf (get-loc 2) (- (get-loc 2) (* 256 mod-num)))
						 (setf (get-loc 0) (+ (get-loc 0) mod-num))))))
	 new-location))

