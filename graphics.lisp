(let* ((main-window nil)
		 (position-buffer-object nil)
		 (vertex-shader nil)
		 (fragment-shader nil)
		 (gl-program nil)
		 (vao-id nil)
		 (height 108)
		 (width 192)
		 ; (* number-of-squares tri-per-square floats-per-tri 3)
		 ; the 3 is bc the number of agents necessarily makes it bigger, and memorys cheap af
		 (gl-array (gl:alloc-gl-array :float  (* height width 2 96 3))))

  (defun string-from-file (path)
	 (with-open-file (file path)
		(let ((data (make-string (file-length file))))
		  (read-sequence data file)
		  data)))

  (defun setup-vertex-buffer (vertex-positions)
	 (gl:bind-buffer :array-buffer position-buffer-object)
	 (gl:buffer-data :array-buffer :static-draw vertex-positions)
	 (gl:bind-buffer :array-buffer 0))

  ;;shader functions:
  (setf vertex-shader (string-from-file "projects/game/data/FragPosition.vert"))

  (setf fragment-shader (string-from-file "projects/game/data/FragPosition.frag"))

  (defun create-shader (shader-type shader-string)
	 (let ((shader (gl:create-shader shader-type))
			 (str-file-data shader-string))
		(gl:shader-source shader str-file-data)
		(gl:compile-shader shader)
		(let* ((pointer (cffi:foreign-alloc :int)))
		  (cl-opengl-bindings:get-shader-iv shader :compile-status pointer)
		  (if (eq (cffi:mem-ref pointer :int) 0)
			 (let* ((infolog (gl:get-shader-info-log shader))
					  (error-string (cond ((eq shader-type :vertex-shader) "vertex")
												 ((eq shader-type :geometry-shader) "geometry")
												 ((eq shader-type :fragment-shader) "fragment")
												 (t "unknown"))))
				(format t "Compile failure in ~A shader:~%~A~%" error-string infolog)))
		  (cffi:foreign-free pointer))
		shader))

  (defun create-program (shader-list)
	 (let ((gl-program (gl:create-program)))
		(dolist (shader shader-list)
		  (gl:attach-shader gl-program shader))
		(gl:link-program gl-program)
		(let ((status (gl:get-program gl-program :link-status)))
		  (if (eq status :false)
			 (let* ((info-log-string (gl:get-program-info-log gl-program))) 
				(format t "Linker failure: ~A~%" info-log-string))))
		(dolist (shader shader-list)
		  (gl:detach-shader gl-program shader))
		gl-program))

  (defun initialize-program ()
	 (let* ((shader-list
				 (list
					(create-shader :vertex-shader vertex-shader)
					(create-shader :fragment-shader fragment-shader)))
			  (gl-program (create-program shader-list)))
		(dolist (shader shader-list)
		  (gl:delete-shader shader))
		gl-program))

  (defun get-color (value)
	 (case value
		((0) '(1.0 0.0 1.0 1.0))
		((1) '(0.5 0.5 0.5 1.0))
		((2) '(1.0 1.0 1.0 1.0))
		((:player) '(0.0 1.0 1.0 1.0))
		((:enemy) '(1.0 0.0 0.0 1.0))
		(otherwise '(0.2 0.2 1.0 1.0))))

  (defun get-tile-color (y x)
	 (get-color (get-tile-value y x)))

  (defun get-verts-colors (y x)
	 (get-tile-color y x))

  (defun get-verts-positions (bottom top left right)
	 (list left top 0.0 1.0 right top 0.0 1.0 left bottom 0.0 1.0 right top 0.0 1.0 left bottom 0.0 1.0 right bottom 0.0 1.0))

  (defun calc-borders (y x height width)
	 (list (- y (/ height 2.0)) (+ y (/ height 2.0)) (- x (/ width 2.0)) (+ x (/ width 2.0))))

  (defun is-onscreen-p (loc top bottom left right)
	 (if (and (>= (nth 0 loc) top)
				 (<= (nth 0 loc) bottom)
				 (>= (nth 1 loc) left)
				 (<= (nth 1 loc) right))
		t
		nil))

  (defun setup-verts (center height width)
	 (let ((square-height (/ 2.0 height))
			 (square-width (/ 2.0 width))
			 (top (+ (nth 0 center) (- (floor (/ height 2.0)))))
			 (bottom (+ (nth 0 center) (ceiling (/ height 2.0))))
			 (left (+ (nth 1 center) (- (floor (/ width 2.0)))))
			 (right (+ (nth 1 center) (ceiling (/ width 2.0))))
			 (vert-index 0)
			 (num-triangles 0))
		(flet ((add-square-positions (y-pos x-pos)
											  (let ((y (* (- y-pos (nth 0 center)) square-height))
													  (x (* (- x-pos (nth 1 center)) square-width)))
												 (dolist (vertex (apply #'get-verts-positions (calc-borders y x square-height square-width)))
													(setf (cffi:mem-aref (gl::gl-array-pointer gl-array) :float vert-index) vertex)
													(setf vert-index (1+ vert-index)))
												 (setf num-triangles (+ 2 num-triangles))))
				 (add-square-colors (colors)
										  (dotimes (repeat 6)
											 (dolist (color colors)
												(setf (cffi:mem-aref (gl::gl-array-pointer gl-array) :float vert-index) color)
												(setf vert-index (1+ vert-index))))))

			 (do ((y-pos top (1+ y-pos)))
				((not (<= y-pos bottom)))
				(do ((x-pos left (1+ x-pos)))
				  ((not (<= x-pos right)))
				  (add-square-positions y-pos x-pos)))

			 (dolist (id (get-agent-id-list))
				(let* ((agent (gethash id (get-agent-table)))
						 (loc (agent-location agent)))
				  (if (is-onscreen-p loc top bottom left right)
					 (add-square-positions (nth 0 loc) (nth 1 loc)))))

			 (do ((y-pos top (1+ y-pos)))
				((not (<= y-pos bottom)))
				(do ((x-pos left (1+ x-pos)))
				  ((not (<= x-pos right)))
				  (add-square-colors (get-verts-colors y-pos x-pos))))

			 (dolist (id (get-agent-id-list))
				(let* ((agent (gethash id (get-agent-table)))
						 (loc (agent-location agent)))
				  (if (is-onscreen-p loc top bottom left right)
					 (add-square-colors (get-color (agent-kind agent))))))

			 (values num-triangles))))

  (defun render (center)
	 (let ((num-triangles (setup-verts center height width)))

		(setup-vertex-buffer gl-array)

		(gl:clear-color 0.5 1.0 0.2 0.0)
		(gl:clear :color-buffer-bit)

		(gl:use-program gl-program)

		(gl:bind-buffer :array-buffer position-buffer-object)

		(setf vao-id (gl:gen-vertex-arrays 1))
		(gl:bind-vertex-array (car vao-id))

		(gl:enable-vertex-attrib-array 0)
		(gl:enable-vertex-attrib-array 1)
		(gl:vertex-attrib-pointer 0 4 :float :false 0 0)
		(gl:vertex-attrib-pointer 1 4 :float :false 0 (* 48 num-triangles))

		(gl:draw-arrays :triangles 0 (* 3 num-triangles))

		(gl:disable-vertex-attrib-array 0)
		(gl:disable-vertex-attrib-array 1)
		(gl:use-program 0)

		(glfw:swap-buffers main-window)))

  (glfw:def-error-callback glfw-error-callback (description)
									(write-line (concatenate 'string "Error: " description) *my-error-log*))

  (glfw:def-window-size-callback glfw-window-size-callback (window width height)
											(declare (ignore window))
											(gl:viewport 0 0 width height))


  (defun initialize-main-window ()

	 (%glfw:init)
	 (glfw:set-error-callback 'glfw-error-callback)

	 (%glfw:window-hint (cffi:foreign-enum-value '%glfw:window-hint :context-version-major) 3)
	 (%glfw:window-hint (cffi:foreign-enum-value '%glfw:window-hint :context-version-minor) 3)
	 (%glfw:window-hint (cffi:foreign-enum-value '%glfw:window-hint :decorated) 0)
	 (%glfw:window-hint (cffi:foreign-enum-value '%glfw:window-hint :resizable) 0)

	 (setf main-window (%glfw:create-window 1280 720 "GPOS" (cffi:null-pointer) (cffi:null-pointer)))

	 (%glfw:make-context-current main-window)

	 (glfw:set-key-callback 'glfw-key-callback main-window)
	 (glfw:set-window-size-callback 'glfw-window-size-callback main-window)

	 (setf gl-program (initialize-program))
	 (setf position-buffer-object (first (gl:gen-buffers 1)))

	 (%glfw:swap-interval 1)
	 main-window)

  (defun close-glfw-and-window ()
	 (%glfw:destroy-window main-window)
	 (setf main-window nil)
	 (%glfw:terminate)))
