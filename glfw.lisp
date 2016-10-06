(ql:quickload "cl-glu")
(ql:quickload "cl-glfw3")

(defun run-game ()
  (load "../../source/projects/game/main.lisp")
  (start))

(defun string-from-file (path)
  (with-open-file (file path)
    (let ((data (make-string (file-length file))))
      (read-sequence data file)
      data)))

(defvar *my-error-log*)

(defun generate-corners (location)
  (declare (ignore location))
  )

(defun fill-gl-array (gl-array data-array)
  "Fills gl-array <gl-array> with <data-array> of type cl:array, <data-array>'s contents
will be COERCEd to SINGLE-FLOAT"
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

(defparameter *verts* #(
     0.0    0.5 0.0 1.0
     0.5 -0.366 0.0 1.0
    -0.5 -0.366 0.0 1.0
     1.0    0.0 0.0 1.0
     0.0    1.0 0.0 1.0
     0.0    0.0 1.0 1.0))

(defparameter *vertex-positions* (create-gl-array-from-vector *verts*))

(defvar *position-buffer-object*)

(defun initialize-vertex-buffer ()
  (setf *position-buffer-object* (first (gl:gen-buffers 1)))
  (gl:bind-buffer :array-buffer *position-buffer-object*)
  (gl:buffer-data :array-buffer :stream-draw *vertex-positions*)
  (gl:bind-buffer :array-buffer 0))

;;shader functions:
(defparameter *vertex-shader* (string-from-file "../../source/projects/game/data/FragPosition.vert"))

(defparameter *fragment-shader* (string-from-file "../../source/projects/game/data/FragPosition.frag"))

(defvar *gl-program-object*)

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

(defvar *gl-program*)
(defvar *shader-list*)

(defun initialize-program ()
  (let* ((shader-list
			 (list
				(create-shader :vertex-shader *vertex-shader*)
				(create-shader :fragment-shader *fragment-shader*)))
			(gl-program (create-program shader-list)))
	 (dolist (shader shader-list)
		(gl:delete-shader shader))
	 gl-program))

(defun input-processing ()
  (%glfw:poll-events))

(defun update ()
  (+ 1 1))

(defvar *vao-id*)

(defun render (window)
  (gl:clear-color 0.0 0.0 0.0 0.0)
  (gl:clear :color-buffer-bit)

  (gl:use-program *gl-program*)

  (gl:bind-buffer :array-buffer *position-buffer-object*)

  (setf *vao-id* (gl:gen-vertex-arrays 1))
  (gl:bind-vertex-array (car *vao-id*))

  (gl:enable-vertex-attrib-array 0)
  (gl:enable-vertex-attrib-array 1)
  (gl:vertex-attrib-pointer 0 4 :float :false 0 0)
  (gl:vertex-attrib-pointer 1 4 :float :false 0 48)

  (gl:draw-arrays :triangles 0 3)

  (gl:disable-vertex-attrib-array 0)
  (gl:disable-vertex-attrib-array 1)
  (gl:use-program 0)
  
  (glfw:swap-buffers window))

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


;; application entry point
(defun start ()

  (%glfw:init)
  (glfw:set-error-callback 'glfw-error-callback)

  (%glfw:window-hint (cffi:foreign-enum-value '%glfw:window-hint :context-version-major) 3)
  (%glfw:window-hint (cffi:foreign-enum-value '%glfw:window-hint :context-version-minor) 3)
  (%glfw:window-hint (cffi:foreign-enum-value '%glfw:window-hint :decorated) 0)
  (%glfw:window-hint (cffi:foreign-enum-value '%glfw:window-hint :resizable) 0)
  
  (let ((main-window (%glfw:create-window 960 540 "GPOS" (cffi:null-pointer) (cffi:null-pointer)))
		  (*my-error-log* (open "error.log" :direction :output :if-exists :supersede)))
	 
	 (%glfw:make-context-current main-window)

	 (glfw:set-key-callback 'glfw-key-callback main-window)
	 (glfw:set-window-size-callback 'glfw-window-size-callback main-window)
	 
	 (setf *gl-program* (initialize-program))

	 (initialize-vertex-buffer)

	 (%glfw:swap-interval 1)
	 (loop
		until (%glfw:window-should-close-p main-window)
		do (input-processing)
		do (update)
		do (render main-window))

	 (close *my-error-log*)

	 (%glfw:destroy-window main-window)
	 (%glfw:terminate)))
