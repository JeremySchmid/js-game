(let ((agent-table (make-hash-table :test 'equal))
		(player-id nil))

  (defun get-agent-table ()
	 agent-table)

  (defclass agent ()
	 ((location :accessor agent-location
					:initarg :location)
	  (visible-agents :accessor agent-visible-agents
							:initform (make-hash-table :test 'equal))
	  (visible-tiles :accessor agent-visible-tiles
						  :initform (make-hash-table :test 'equal))
	  (collision :accessor agent-collision
					 :initarg :collision)
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

  (defun agent-solid-p (agent)
	 (agent-collision agent))

  (defun check-empty-p (target-loc)
	 (and (floor-p (get-tile-value (abs-of (y-of target-loc)) (abs-of (x-of target-loc))))
			(not (member target-loc (mapcar #'agent-location (remove-if-not #'agent-solid-p (hash-values agent-table))) :test 'equal))))

  (defun gen-enemy-loc ()
	 (let ((prospect (point (coord (random 20) 0.0) (coord (random 20) 0.0))))
		(if (check-empty-p prospect)
		  prospect
		  (gen-enemy-loc))))

  (defun make-agent (kind)
	 (case kind
		((:player) (make-instance 'agent
										  :location (point (coord 1 0.0) (coord 1 0.0))
										  :collision t
										  :damage '(3 0 0)
										  :health '(20 7)
										  :max-health '(20 7)
										  :regen '(0.33 0.5)
										  :defense '(1 0 0)
										  :effects nil
										  :kind kind))
		((:item) (make-instance 'agent
										:location (gen-enemy-loc)
										:collision nil
										:damage '(0 0 0)
										:health '(100 25)
										:max-health '(100 25)
										:regen '(2.0 1.0)
										:defense '(2 1 1)
										:effects nil
										:kind kind))
		((:enemy t) (make-instance 'agent
											:location (gen-enemy-loc)
											:collision t
											:damage '(1 0 0)
											:health '(5 2)
											:max-health '(5 2)
											:regen '(.34 0.17)
											:defense '(0 0 0)
											:effects nil
											:kind kind))))

  (defun initialize-agent (kind)
	 (let* ((agent (make-agent kind))
			  (id (agent-id agent)))
		(setf (gethash id agent-table) agent)
		(if (eq kind :player)
		  (setf player-id id))))

  (defun compute-visible-agents (visible-agents-table origin
																		&optional (visible-tiles-table (full-compute-visible-tiles (make-hash-table :test 'equal) origin)))
	 (let ((visible-tiles (hash-keys visible-tiles-table)))
		(dolist (agent (hash-values agent-table))
		  (let ((agent-loc (agent-location agent)))
			 (if (member agent-loc visible-tiles :test 'equal)
				(setf (gethash agent-loc visible-agents-table) agent))))))

  (defun update-agent (agent)
	 (setf (agent-visible-tiles agent) (make-hash-table :test 'equal))
	 (full-compute-visible-tiles (agent-visible-tiles agent) (agent-location agent))
	 (setf (agent-visible-agents agent) (make-hash-table :test 'equal))
	 (compute-visible-agents (agent-visible-agents agent) (agent-location agent) (agent-visible-tiles agent)))

  (defun update-agents ()
	 (dolist (agent (hash-values agent-table))
		(update-agent agent)))

  (defun get-player ()
	 (gethash player-id agent-table))

  (defun move-direction (loc num)
	 (point (case num
				 ((1 2 3) (add-distance-to-coord -1 (y-of loc)))
				 ((7 8 9) (add-distance-to-coord 1 (y-of loc)))
				 (t (y-of loc)))
			  (case num
				 ((1 4 7) (add-distance-to-coord -1 (x-of loc)))
				 ((3 6 9) (add-distance-to-coord 1 (x-of loc)))
				 (t (x-of loc)))))

  (defun move (agent num)
	 (let ((target-loc (move-direction (agent-location agent) num)))
		(if (check-empty-p target-loc)
		  (setf (agent-location agent) target-loc))))

  )
