(ql:quickload "cl-glu")
(ql:quickload "cl-glfw3")
(ql:quickload "alexandria")

(let ((main-window nil)
		(verts nil)
		(vertex-positions nil)
		(position-buffer-object nil)
		(vertex-shader nil)
		(fragment-shader nil)
		(gl-program nil)
		(vao-id nil)
		(height 54)
		(width 96))

  (defun string-from-file (path)
	 (with-open-file (file path)
		(let ((data (make-string (file-length file))))
		  (read-sequence data file)
		  data)))

  (defun fill-gl-array (gl-array data-array)
	 "Fills gl-array <gl-array> with <data-array> of type cl:array, <data-array>'s contents will be COERCEd to SINGLE-FLOAT"
	 (if (and (typep gl-array 'gl:gl-array)
				 (arrayp data-array)
				 (= (gl::gl-array-size gl-array)
					 (length data-array)))
		(dotimes (i (length data-array))
		  (setf
			 (gl:glaref gl-array i)
			 (coerce (aref data-array i) 'single-float)))
		(progn
		  (print "couldn't fill gl-array, either size of gl-array and data-array don't")
		  (print "match or they aren't of proper type"))))

  (defun create-gl-array-from-vector (vector-of-floats)
	 (let* ((array-length (length vector-of-floats))
			  (gl-array (gl:alloc-gl-array :float array-length)))
		(fill-gl-array gl-array vector-of-floats)
		gl-array))

  (defun setup-vertex-buffer ()
	 (setf position-buffer-object (first (gl:gen-buffers 1)))
	 (gl:bind-buffer :array-buffer position-buffer-object)
	 (gl:buffer-data :array-buffer :stream-draw vertex-positions)
	 (gl:bind-buffer :array-buffer 0))

  ;;shader functions:
  (setf vertex-shader (string-from-file "../../source/projects/game/data/FragPosition.vert"))

  (setf fragment-shader (string-from-file "../../source/projects/game/data/FragPosition.frag"))

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

  (defun input-processing ()
	 (%glfw:poll-events))

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

  (defun get-verts-color-sequence (y x)
	 (let ((a (get-tile-color y x)))
		(concatenate 'list a a a a a a)))

  (defun get-verts-position-sequence (bottom top left right)
	 (list top left 0.0 1.0 top right 0.0 1.0 bottom left 0.0 1.0 top right 0.0 1.0 bottom left 0.0 1.0 bottom right 0.0 1.0))

  (defun calc-square-borders (y x height width)
	 (list (- y (/ height 2.0)) (+ y (/ height 2.0)) (- x (/ width 2.0)) (+ x (/ width 2.0))))

  (defun setup-verts (center height width)
	 (let ((square-height (/ 2.0 height))
			 (square-width (/ 2.0 width))
			 (square-list nil)
			 (color-list nil)
			 (center-y (- 0.0 (aref center 2)))
			 (center-x (- 0.0 (aref center 3)))
			 (num-triangles 0))
		(do ((y-offset (- (floor (/ height 2.0))) (1+ y-offset)))
		  ((not (<= y-offset (ceiling (/ height 2.0)))))
		  (do ((x-offset (- (floor (/ width 2.0))) (1+ x-offset)))
		    ((not (<= x-offset (ceiling (/ width 2.0)))))
			 (let ((y (+ center-y (* y-offset square-height)))
					 (x (+ center-x (* x-offset square-width))))
				(push (apply #'get-verts-position-sequence (calc-square-borders y x square-height square-width)) square-list)
				(push (get-verts-color-sequence (+ y-offset (aref center 0)) (+ x-offset (aref center 1))) color-list))
				(setf num-triangles (+ 2 num-triangles))))
		(setf verts (concatenate 'vector (alexandria:flatten square-list) (alexandria:flatten color-list)))
		(values num-triangles)))

  (defun render (&key (center (get-player-loc)))
	 (let ((num-triangles (setup-verts center height width)))

		(setf vertex-positions (create-gl-array-from-vector verts))

		(setup-vertex-buffer)

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

  ;; replace with a "key-processor" adding pressed (repeated?) and released keys to a/the queue used for the input, maybe with a time-tag for when it was done
  ;; input processing will need to be dependent on whether game is in realtime or turnbased mode -> needs to be done in the queue processing, not this callback function - processing should either be done in the update portion of the loop before the physics update or in a function before (update) - model-view-controller separation
  ;; view is the opengl stuff->translates physics data to onscreen stuff
  ;; model is the worlddata and physics rules/functions->updates the world
  ;; controller is the input stuff->translates the multiple possible input-types into the actions/flags/functions - what the model should do

  (glfw:def-key-callback glfw-key-callback (window key scancode action mods)
								 (declare (ignore scancode mods))
								 (if (and (eq key :escape)
											 (or (eq action :press)
												  (eq action :repeat)))
									(%glfw:set-window-should-close window t)))

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

	 (setf main-window (%glfw:create-window 960 540 "GPOS" (cffi:null-pointer) (cffi:null-pointer)))

	 (%glfw:make-context-current main-window)

	 (glfw:set-key-callback 'glfw-key-callback main-window)
	 (glfw:set-window-size-callback 'glfw-window-size-callback main-window)

	 (setf gl-program (initialize-program))

	 (%glfw:swap-interval 1)
	 main-window)

  (defun close-glfw-and-window ()
	 (%glfw:destroy-window main-window)
	 (setf main-window nil)
	 (%glfw:terminate)))
