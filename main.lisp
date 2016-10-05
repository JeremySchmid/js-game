(defun run-game ()
  (load "source/game.lisp")
  (start))

(defvar *my-error-log*)

(defun update ()
  (+ 1 1))

(defun render ())

(defparameter element-list '((1 (floor)) (2 (wall))))

(defun initialize-map-area (width height)
  (let ((area (make-array (list width height) :element-type 'fixnum :initial-element 1)))
	 (do ((x 0 (1+ x)))
		  ((>= x width))
	   (do ((y 0 (1+ y)))
		    ((>= y height))
		  (if (or (= x 0) (= y 0) (= x (1- width)) (= y (1- height)))
			 (setf (aref area x y) 2))))
	 area))

(defun print-square (row square)
  (cond ((equal player (cons row square)) "@")
		  ((= square 1) ".")
		  ((= square 2) "0")
		  (t " ")))

(defun print-row ()
  (mapcar #'print-square

(defun print-map-area (map-area player)
  (loop for row from 0 to 
		  do (loop for square in row
					  do (let ((square-char (cond ((equal player (cons row square)) "@")
														((= square 1) ".")
														((= square 2) "0")
														(t " "))))
						 (princ square-char)))
		  (princ #\NewLine)))


(defun start ()
  (with-open-file (*my-error-log* "error.log" :direction :output :if-exists :supersede)
	 (let ((map-area `(,(initialize-map-area 220 80) 220 80))
			 (player '(75 . 25)))
		(print-map-area map-area player))))
