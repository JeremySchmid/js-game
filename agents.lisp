(let ((agent-table (make-hash-table :test 'equal))
		(agent-id-list nil)
		(player-id nil))

  (defun get-agent-id-list ()
	 agent-id-list)

  (defun get-agent-table ()
	 agent-table)

  (defclass agent ()
	 ((location :accessor agent-location
					:initarg :location)
	  (damage :accessor agent-damage
				 :initarg :damage)
	  (health :accessor agent-health
				 :initarg :health)
	  (max-health :accessor agent-max-health
				 :initarg :max-health)
	  (regen :accessor agent-regen
				:initarg :regen)
	  (defense :accessor agent-defense
				 :initarg :defense)
	  (id :accessor agent-id
			  :initform (gensym)
			  :initarg :id)
	  (effects :accessor agent-effects
				  :initarg :effects)
	  (kind :accessor agent-kind
			  :initarg :kind)))

  (defun make-agent (kind location)
	 (case kind
		((:player) (make-instance 'agent
										  :location location
										  :damage #(3 0 0)
										  :health #(20 7)
										  :max-health #(20 7)
										  :regen #(0.33 0.5)
										  :defense #(1 0 0)
										  :effects nil
										  :kind kind))
		((:object) (make-instance 'agent
										  :location location
										  :damage #(0 0 0)
										  :health #(100 25)
										  :max-health #(100 25)
										  :regen #(2.0 1.0)
										  :defense #(2 1 1)
										  :effects nil
										  :kind kind))
		(t (make-instance 'agent
								:location location
								:damage #(1 0 0)
								:health #(5 2)
								:max-health #(5 2)
								:regen #(.34 0.17)
								:defense #(0 0 0)
								:effects nil
								:kind kind))))
  
  (defun initialize-agent (kind location)
	 (let* ((agent (make-agent kind location))
			  (id (agent-id agent)))
		(push id agent-id-list)
		(setf (gethash id agent-table) agent)
		(if (eq kind :player)
		  (setf player-id id))))
  
  (defun update-agent (id agent)
	 (declare (ignore id agent)))

  (defun update-agents ()
	 (loop for id in agent-id-list
			 do (update-agent id (gethash id agent-table))))
  
  (defun get-player ()
	 (gethash player-id agent-table))

  (defun move-direction (loc num)
	 (let ((vec loc))
		(case num
		  ((1 2 3) (setf (aref vec 0) (- (aref loc 0) 1)))
		  ((7 8 9) (setf (aref vec 0) (+ (aref loc 0) 1))))
		(case num
		  ((1 4 7) (setf (aref vec 1) (- (aref loc 1) 1)))
		  ((3 6 9) (setf (aref vec 1) (+ (aref loc 1) 1))))
		(values vec)))
	 

  (defun walk (num)
	 (let ((player (get-player)))
		(setf (agent-location player) (move-direction (agent-location player) num))
		(render-map 30)))
  )
