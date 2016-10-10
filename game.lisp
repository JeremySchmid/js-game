(defparameter *path-prefix* "projects/game/")
(defparameter *my-error-log* nil)
(defparameter *my-main-window* nil)

(load (concatenate 'string *path-prefix* "player.lisp"))
(load (concatenate 'string *path-prefix* "graphics.lisp"))
(load (concatenate 'string *path-prefix* "maps.lisp"))

(declaim (optimize (debug 3) (safety 3)))

(defun initialize-game ()
  (setf *my-error-log* (open "error.log" :direction :output :if-exists :supersede))
  (initialize-main-window))

(defun update ()
  ())

(defun close-down ()
  (close *my-error-log*)
  (close-glfw-and-window))

(defun run-game (main-window)
  (unless (%glfw:window-should-close-p main-window)
	 (input-processing)
	 (update)
	 (render (get-player-loc))
	 (run-game main-window)))

(defun start ()
  (load (concatenate 'string *path-prefix* "game.lisp"))
  (let ((main-window (initialize-game)))
	 (run-game main-window))
  (close-down))
