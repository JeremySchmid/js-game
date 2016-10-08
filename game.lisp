
(defparameter *path-prefix* "projects/game/")

(load (concatenate 'string *path-prefix* "graphics.lisp"))
(load (concatenate 'string *path-prefix* "maps.lisp"))
(load (concatenate 'string *path-prefix* "player.lisp"))

(defparameter *my-error-log* nil)

(defparameter *element-list* '((1 (floor)) (2 (wall))))

(defun render (size)
  (print-area (get-player-loc)
					 (vector (- (aref (get-player-loc) 0) size)
								(+ (aref (get-player-loc) 0) size))
					 (vector (- (aref (get-player-loc) 1) size)
								(+ (aref (get-player-loc) 1) size))))

(defun start ()
  (with-open-file (*my-error-log* (concatenate 'string *path-prefix* "error.log")
											 :direction :output
											 :if-exists :supersede)
	 (render 35)))

(defun run-game ()
  (load (concatenate 'string *path-prefix* "game.lisp"))
  (start))
