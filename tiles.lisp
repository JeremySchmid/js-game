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
  (let* ((point (subtract-points point2 point1))
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

(defun blocks-light-p (y x quadrant origin)
  (let ((new-y (abs-of (y-of origin)))
		  (new-x (abs-of (x-of origin))))
	 (case quadrant
		((0) (incf new-x x) (incf new-y y))
		((1) (decf new-x y) (incf new-y x))
		((2) (incf new-x y) (decf new-y x))
		((3) (decf new-x x) (decf new-y y)))
	 (not (transparent-p (pair new-y new-x)))))

(defun set-visible-tile (visible-table tile)
  (setf (gethash tile visible-table) t))

(defun set-visible (visible-table y x quadrant origin)
  (let ((new-y (abs-of (y-of origin)))
		  (new-x (abs-of (x-of origin))))
	 (case quadrant
		((0) (incf new-x x) (incf new-y y))
		((1) (decf new-x y) (incf new-y x))
		((2) (incf new-x y) (decf new-y x))
		((3) (decf new-x x) (decf new-y y)))
	 (set-visible-tile visible-table (pair new-y new-x))))

(defmacro slope-compare (func pair1 pair2)
  `(,func (* (y-of ,pair1) (x-of ,pair2)) (* (x-of ,pair1) (y-of ,pair2))))

(defun compute-visible-tiles (visible-table quadrant origin max-range x-start)

  (loop for x from x-start to max-range
		  do (loop for y from x-start to max-range
					  do (set-visible visible-table y x quadrant origin))))

(defun full-compute-visible-tiles (visible-table origin)
  (let ((max-range 50))
	 (set-visible-tile visible-table (point (abs-of (y-of origin)) (abs-of (x-of origin))))
	 (loop for quadrant from 0 to 3
			 do (compute-visible-tiles visible-table quadrant origin max-range 0)))
  visible-table)


(defun print-square (loc row column)
  (princ (if (and (equal (abs-of (y-of loc)) row)
						(equal (abs-of (x-of  loc)) column))
			  (tile-value-char 0)
			  (tile-value-char (get-tile-value row column)))))

(defun print-row (loc row x-borders)
  (do ((column (first x-borders) (1+ column))) 
	 ((> column (second x-borders)))
	 (print-square loc row column)))

(defun print-area (loc y-borders x-borders)
  (do ((row (second y-borders) (1- row)))
	 ((< row (first y-borders)))
	 (print-row loc row x-borders)
	 (format t "~%")))

(defun render-map (loc size)
  (print-area loc
				  (list (- (abs-of (y-of loc)) size)
							 (+ (abs-of (y-of loc)) size))
				  (list (- (abs-of (x-of loc)) size)
							 (+ (abs-of (x-of loc)) size))))

