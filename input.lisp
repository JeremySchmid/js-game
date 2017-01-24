(defun input-processing ()
  (%glfw:poll-events))

  ;; replace with a "key-processor" adding pressed (repeated?) and released keys to a/the queue used for the input, maybe with a time-tag for when it was done
  ;; input processing will need to be dependent on whether game is in realtime or turnbased mode -> needs to be done in the queue processing, not this callback function - processing should either be done in the update portion of the loop before the physics update or in a function before (update) - model-view-controller separation
  ;; view is the opengl stuff->translates physics data to onscreen stuff
  ;; model is the worlddata and physics rules/functions->updates the world
  ;; controller is the input stuff->translates the multiple possible input-types into the actions/flags/functions - what the model should do
(glfw:def-key-callback
  glfw-key-callback (window key scancode action mods)
  (declare (ignore scancode mods))
	 (if (eq key :escape)
		(%glfw:set-window-should-close window t)
		(if (or (eq action :press)
				  (eq action :repeat))
		  (setf *update-queue* (append *update-queue* (list (list key)))))))
