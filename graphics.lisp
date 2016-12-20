(let* ((main-window nil)
		 (position-buffer-object nil)
		 (vertex-shader nil)
		 (fragment-shader nil)
		 (gl-program nil)
		 (vao-id nil)
		 (window-height 360)
		 (window-width 640)
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
	 (gl:buffer-data :array-buffer :dynamic-draw vertex-positions)
	 (gl:bind-buffer :array-buffer 0))

  ;;shader functions:
  (setf vertex-shader (string-from-file "./data/FragPosition.vert"))

  (setf fragment-shader (string-from-file "./data/FragPosition.frag"))

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
		((1) '(0.3 0.3 0.3 1.0))
		((2) '(1.0 1.0 1.0 1.0))
		((:player) '(0.0 1.0 1.0 1.0))
		((:enemy) '(1.0 0.0 0.0 1.0))
		((:item) '(1.0 1.0 0.0 1.0))
		(otherwise '(0.2 0.2 1.0 1.0))))

  (defun get-verts-positions (bottom top left right)
	 (list left top 0.0 1.0 right top 0.0 1.0 left bottom 0.0 1.0 right top 0.0 1.0 left bottom 0.0 1.0 right bottom 0.0 1.0))

  (defun calc-borders (y x y-float x-float height width)
	 (list (- (+ y y-float) (/ height 2.0)) (+ (+ y y-float) (/ height 2.0)) (- (+ x x-float) (/ width 2.0)) (+ (+ x x-float) (/ width 2.0))))

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
			 (bottom (+ (abs-of (y-of center)) (- (floor (/ height 2.0)))))
			 (top (+ (abs-of (y-of center)) (ceiling (/ height 2.0))))
			 (left (+ (abs-of (x-of center)) (- (floor (/ width 2.0)))))
			 (right (+ (abs-of (x-of center)) (ceiling (/ width 2.0))))
			 (vert-index 0)
			 (num-triangles 0))
		(flet ((add-square-positions (location height width)
											  (let ((y (* (- (abs-of (y-of location)) (abs-of (y-of center))) square-height))
													  (x (* (- (abs-of (x-of location)) (abs-of (x-of center))) square-width))
													  (y-float (* (- (rel-of (y-of location)) (rel-of (y-of center))) square-height))
													  (x-float (* (- (rel-of (x-of location)) (rel-of (x-of center))) square-height)))
												 (dolist (vertex (apply #'get-verts-positions (calc-borders y x y-float x-float height width)))
													(setf (cffi:mem-aref (gl::gl-array-pointer gl-array) :float vert-index) vertex)
													(incf vert-index))
												 (incf num-triangles 2)))
				 (add-square-colors (colors)
										  (dotimes (repeat 6)
											 (dolist (color colors)
												(setf (cffi:mem-aref (gl::gl-array-pointer gl-array) :float vert-index) color)
												(incf vert-index)))))

		  (loop for y-pos from bottom to top
				  do (loop for x-pos from left to right
							  do (if (gethash (pair y-pos x-pos) (agent-visible-tiles (get-player)))
									 (add-square-positions (point (coord y-pos 0.0) (coord x-pos 0.0)) square-height square-width)
									 (if (not (= 0 (get-tile-memory y-pos x-pos)))
										(add-square-positions (point (coord y-pos 0.0) (coord x-pos 0.0)) square-height square-width)))))

		  (let ((visible-agents (agent-visible-agents (get-player))))
			 (dolist (agent (hash-values visible-agents))
				(let ((loc (agent-location agent)))
					 (add-square-positions loc square-height square-width))))

		  (loop for y-pos from bottom to top
				  do (loop for x-pos from left to right
							  do (if (gethash (pair y-pos x-pos) (agent-visible-tiles (get-player)))
									 (add-square-colors (get-color (get-tile-value y-pos x-pos)))
									 (if (not (= 0 (get-tile-memory y-pos x-pos)))
										(add-square-colors (mapcar #'* (get-color (get-tile-memory y-pos x-pos)) '(0.5 0.5 0.5 1.0)))))))



		  (let ((visible-agents (agent-visible-agents (get-player))))
			 (dolist (agent (hash-values visible-agents))
				  (add-square-colors (get-color (agent-kind agent)))))

		  (values num-triangles))))

  (defun render (center)
	 (let ((num-triangles (setup-verts center height width)))

		(setup-vertex-buffer gl-array)

		(gl:clear-color 0.0 0.0 0.0 1.0)
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

	 (setf main-window (%glfw:create-window window-width window-height "GPOS" (cffi:null-pointer) (cffi:null-pointer)))

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
