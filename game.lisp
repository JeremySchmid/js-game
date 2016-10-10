(defparameter *path-prefix* "projects/game/")
(defparameter *my-error-log* nil)
(defparameter *my-main-window* nil)

(load (concatenate 'string *path-prefix* "graphics.lisp"))
(load (concatenate 'string *path-prefix* "maps.lisp"))
(load (concatenate 'string *path-prefix* "player.lisp"))

(defun initialize-game ()
  (setf *my-error-log* (open "error.log" :direction :output :if-exists :supersede))
  (initialize-main-window))

(defun close-down ()
  (close *my-error-log*)
  (close-glfw-and-window))

(defun render-map (size)
  (print-area (get-player-loc)
					 (vector (- (aref (get-player-loc) 0) size)
								(+ (aref (get-player-loc) 0) size))
					 (vector (- (aref (get-player-loc) 1) size)
								(+ (aref (get-player-loc) 1) size))))

(defun run-game (main-window)
  (unless (%glfw:window-should-close-p main-window)
	 (input-processing)
	 (update)
	 (render)
	 (run-game main-window)))

(defun start ()
  (load (concatenate 'string *path-prefix* "game.lisp"))
  (let ((main-window (initialize-game)))
	 (run-game main-window))
  (close-down))
