(defun run-game ()
  (load "../../source/projects/game/main.lisp")
  (start))

(defvar *my-error-log*)

(defun update ()
  (+ 1 1))

(defun render ())

(defparameter element-list '((1 (floor)) (2 (wall))))

(defun initialize-map-area (height width)
  (let ((area (make-array (list height width) :element-type 'fixnum :initial-element 1)))
	 (do ((y 0 (1+ y)))
		((>= y height))
		(do ((x 0 (1+ x)))
		  ((>= x width))
		  (if (or (= x 0) (= y 0) (= x (1- width)) (= y (1- height)))
			 (setf (aref area y x) 2))))
	 area))

(defun print-square (verts player-loc row column)
  (let ((value (aref verts row column)))
	 (format t (cond ((equal player-loc (cons row column)) "@")
					  ((= value 1) " ")
					  ((= value 2) "-")
					  (t "e")))))

(defun print-row (verts player-loc row num-columns)
  (loop for column from 0 to (1- num-columns)
		  do (print-square verts player-loc row column)))

(defun print-map-area (map-area player-loc)
  (let ((num-rows (nth 1 map-area)))
	 (loop for row from 0 to (1- num-rows)
			 do (print-row (nth 0 map-area) player-loc row (nth 2 map-area))
			 (format t "~%"))))

(defun start ()
  (with-open-file (*my-error-log* "error.log" :direction :output :if-exists :supersede)
	 (let ((map-area (list (initialize-map-area 80 220) 80 220))
			 (player '(25 . 75)))
		(list map-area player)
		(print-map-area map-area player))))
