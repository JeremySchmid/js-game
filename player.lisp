(let ((player-loc #(25 25 0.3 -0.2)))

  (defun get-player-loc ()
	 (values player-loc))

  (defun walk (num)
	 (case num
		((7 8 9) (setf (aref player-loc 2) (- (aref player-loc 2) 1.0)))
		((1 2 3) (setf (aref player-loc 2) (+ (aref player-loc 2) 1.0))))
	 (case num
		((1 4 7) (setf (aref player-loc 3) (- (aref player-loc 3) 1.0)))
		((3 6 9) (setf (aref player-loc 3) (+ (aref player-loc 3) 1.0))))
	 (setf player-loc (recanon-loc player-loc))
	 (render 20))
  )
