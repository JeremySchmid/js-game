(defun pair (y x)
  (list y x))

(defun first-of (pair)
  (first pair))

(defun second-of (pair)
  (second pair))

(defun coord (abs rel)
  (pair abs rel))

(defun abs-of (coord)
  (first coord))

(defun rel-of (coord)
  (second coord))

(defun add-coords (coord1 coord2)
  (coord (+ (abs-of coord1) (abs-of coord2)) (+ (rel-of coord1) (rel-of coord2))))

(defun subtract-coords (coord1 coord2)
  (coord (- (abs-of coord1) (abs-of coord2)) (- (rel-of coord1) (rel-of coord2))))

(defun *-coords (coord1 coord2)
  (+ (* (abs-of coord1) (abs-of coord2)) (* (abs-of coord1) (rel-of coord2)) (* (rel-of coord1) (abs-of coord2)) (* (rel-of coord1) (rel-of coord2))))

(defun coord<-float (float)
  (multiple-value-bind (a b) (floor float)
	 (coord a b)))

(defun float<-coord (coord)
  (+ (abs-of coord) (rel-of coord)))

(defun recanon-coord (coord)
  (let ((abs (abs-of coord))
		  (rel (rel-of coord)))
	 (multiple-value-bind (a b) (floor rel)
		(coord (+ abs a) b))))

(defun add-distance-to-coord (number coord)
  (recanon-coord (coord (abs-of coord) (+ number (rel-of coord)))))

(defun point (y-coord x-coord)
  (pair y-coord x-coord))

(defun y-of (point)
  (first point))

(defun x-of (point)
  (second point))

(defun add-points (p1 p2)
  (point (add-coords (y-of p1) (y-of p2)) (add-coords (x-of p1) (x-of p2))))

(defun subtract-points (p1 p2)
  (point (subtract-coords (y-of p1) (y-of p2)) (subtract-coords (x-of p1) (x-of p2))))

(defun recanon-point (point)
  (let ((y (y-of point))
		  (x (x-of point)))
	 (point (recanon-coord y) (recanon-coord x))))

(defun distance (point)
  (let* ((y (y-of point))
			(x (x-of point)))
	 (sqrt (+ (*-coords y y) (*-coords x x)))))
  
(defun get-simple-distance (pair)
  (let ((y (y-of pair))
		  (x (x-of pair)))
  (sqrt (+ (* y y) (* x x)))))

(defun get-slope (point)
  (/ (float<-coord (y-of point)) (float<-coord (x-of point))))


(defun line (point1 point2)
  (let ((slope-point (subtract-points point2 point1))
		  (slope (get-slope point)))
  (pair slope point1)))

(defun slope-of (line)
  (first line))

(defun y-int-of (line)
  (second line))

(defun floor-p (value)
  (logbitp 0 value))

(defun wall-p (value)
  (not (logbitp 0 value)))

(defun transparent-p (loc)
  (let ((value (get-tile-value (y-of loc) (x-of loc))))
	 (floor-p value)))

(defun blocks-light-p (y x octant origin)
  (let ((new-y (abs-of (y-of origin)))
		  (new-x (abs-of (x-of origin))))
	 (case octant
		((0) (incf new-x x) (decf new-y y))
		((1) (incf new-x y) (decf new-y x))
		((2) (decf new-x y) (decf new-y x))
		((3) (decf new-x x) (decf new-y y))
		((4) (decf new-x x) (incf new-y y))
		((5) (decf new-x y) (incf new-y x))
		((6) (incf new-x y) (incf new-y x))
		((7) (incf new-x x) (incf new-y y)))
	 (not (transparent-p (pair new-y new-x)))))

(defun set-visible-tile (visible-table tile)
  (setf (gethash tile visible-table) t))

(defun set-visible (visible-table y x octant origin)
  (let ((new-y (abs-of (y-of origin)))
		  (new-x (abs-of (x-of origin))))
	 (case octant
		((0) (incf new-x x) (decf new-y y))
		((1) (incf new-x y) (decf new-y x))
		((2) (decf new-x y) (decf new-y x))
		((3) (decf new-x x) (decf new-y y))
		((4) (decf new-x x) (incf new-y y))
		((5) (decf new-x y) (incf new-y x))
		((6) (incf new-x y) (incf new-y x))
		((7) (incf new-x x) (incf new-y y)))
	 (set-visible-tile visible-table (pair new-y new-x))))

(defmacro slope-relation (func point1 point2)
  `(,func (*-coords (y-of ,point1) (x-of ,point2)) (*-coords (x-of ,point1) (y-of ,point2))))
  
(defun compute-visible-tiles (visible-table octant origin max-range x-start top bottom)
  (block main-func
			(loop for x-distance from x-start to (ceiling max-range)
					do (let ((x (coord x-distance 0.0))
								(bottom-origin (coord-from-float -0.5))
								(right-origin (coord-from-float 0.5))
								(top-origin (coord-from-float 0.5))
								(left-tile (coord-from-float (+ x-distance -0.5)))
								(was-opaque -1))

						  (setf top-y (line (point bottom-origin right-origin) (point (add-distance-to-coord -0.5 x) left-tile)))
						  (setf bottom-y (line (point top-origin right-origin) (point (add-distance-to-coord 0.5 x) left-tile)))
						  (if (= 1 (float<-coord (x-of top)))
							 (setf top-y x)
							 (progn (setf top-y (coord<-float (/ (+ (* (1- (* 2 (float<-coord x)))
																					 (float<-coord (y-of top)))
																				 (float<-coord (x-of top)))
																			 (* 2 (float<-coord (x-of top))))))
									  (unless (blocks-light-p (abs-of top-y) (abs-of x) octant origin)
										 (let ((ax (add-distance-to-coord 0.5 x))
												 (ay (add-distance-to-coord 0.5 top-y)))
											(if (slope-relation > top (point ay ax))
											  (setf top-y ay))))))
						  (unless (= 0 (float<-coord (y-of bottom)))
							 (setf bottom-y (coord<-float (/ (+ (* (1- (* 2 (float<-coord x)))
																				(float<-coord (y-of bottom)))
																			(float<-coord (x-of bottom)))
																		(* 2 (float<-coord (x-of bottom)))))))
							 (do ((y top-y (add-distance-to-coord -1 y)))
								((not (slope-relation >= (point y x) (point bottom-y x))))
								(if (or (< max-range 0) (<= (distance (point y x)) max-range))
								  (let* ((is-opaque (blocks-light-p (abs-of y) (abs-of x) octant origin))
											(is-visible (or is-opaque
																 (and (or (not (equal y top-y))
																			 (slope-relation >= top (point y x)))
																		(or (not (equal y bottom-y))
																			 (slope-relation <= bottom (point y x)))))))
									 (if is-visible
										(set-visible visible-table (abs-of y) (abs-of x) octant origin))
									 (if (not (= (abs-of x) max-range))
										(if is-opaque
										  (progn (if (= was-opaque 0)
													  (let ((nx (add-distance-to-coord -0.5 x))
															  (ny (add-distance-to-coord 0.5 y)))
														 (if (slope-relation > top (point ny nx))
															(if (equal y bottom-y)
															  (progn (setf bottom (pair ny nx))
																		(return))
															  (compute-visible-tiles visible-table octant origin max-range
																							 (+ (abs-of x) 1) top (pair ny nx)))
															(if (equal y bottom-y)
															  (return-from main-func)))))
													(setf was-opaque 1))
										  (progn (if (> was-opaque 0)
													  (let ((nx (add-distance-to-coord 0.5 x))
															  (ny (add-distance-to-coord 0.5 y)))
														 (if (slope-relation >= bottom (point ny nx))
															(return-from main-func))
														 (setf top (pair ny nx))))
													(setf was-opaque 0)))))))
						  (if (not (= was-opaque 0))
							 (return))))))
#|
(defun compute-visible-tiles_ (visible-table octant origin max-range x-start top bottom)
  (block main-func
			(loop for x from x-start to max-range
					do (let ((top-y 0)
								(bottom-y 0)
								(was-opaque -1))
						  (if (= 1 (x-of top))
							 (setf top-y x)
							 (progn (setf top-y (floor (/ (+ (x-of top) (* (y-of top) (1- (* x 2)))) (* 2 (x-of top)))))))
						  (unless (= 0 (y-of bottom))
							 (progn (setf bottom-y (floor (/ (+ (x-of bottom) (* (y-of bottom) (1- (* 2 x)))) (* 2 (x-of bottom)))))
									  (if (and (slope-greater-or-equal bottom (1+ (* 2 bottom-y)) (* 2 x))
												  (blocks-light-p bottom-y x octant origin)
												  (not (blocks-light-p (1+ bottom-y) x octant origin)))
										 (incf bottom-y))))
							 (do ((y top-y (1- y)))
								((not (>= y bottom-y)))
								(if (or (< max-range 0) (<= (get-simple-distance (pair y x)) max-range))
								  (let* ((is-opaque (blocks-light-p y x octant origin))
											(is-visible (or is-opaque
																 (and (or (not (= y top-y))
																			 (slope-greater-or-equal top y x))
																		(or (not (= y bottom-y))
																			 (slope-less-or-equal bottom y x))))))
									 (if is-visible
										(set-visible visible-table y x octant origin))
									 (if (not (= x max-range))
										(if is-opaque
										  (progn (if (= was-opaque 0)
													  (let ((nx (* 2 x))
															  (ny (1+ (* 2 y))))
														 (if (blocks-light-p (1+ y) x octant origin)
															(decf nx))
														 (if (slope-greater top ny nx)
															(if (= y bottom-y)
															  (progn (setf bottom (pair ny nx))
																		(return))
															  (compute-visible-tiles visible-table octant origin max-range
																							 (1+ x) top (pair ny nx)))
															(if (= y bottom-y)
															  (return-from main-func)))))
													(setf was-opaque 1))
										  (progn (if (> was-opaque 0)
													  (let ((nx (* 2 x))
															  (ny (1+ (* 2 y))))
														 (if (blocks-light-p (1+ y) (1+ x) octant origin)
															(incf nx))
														 (if (slope-greater-or-equal bottom ny nx)
															(return-from main-func))
														 (setf top (pair ny nx))))
													(setf was-opaque 0)))))))
						  (if (not (= was-opaque 0))
							 (return))))))
|#

(defun full-compute-visible-tiles (visible-table origin)
  (let ((range 50))
	 (set-visible-tile visible-table (point (abs-of (y-of origin)) (abs-of (y-of origin))))
	 (loop for octant from 0 to 7
			 do (compute-visible-tiles visible-table octant origin range 1
												(point (coord 1 0.0) (coord 1 0.0))
												(point (coord 0 0.0) (coord 1 0.0)))))
  visible-table)
