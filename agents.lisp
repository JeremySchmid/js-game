
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
			  :initarg :kind)
	  (ticks :accessor agent-ticks
				:initarg :ticks
				:initform 0)
	  (inventory :accessor agent-inventory
					 :initarg nil
					 :initform :inventory)))

(let ((agent-table (make-hash-table :test 'equal))
		(move-ticks-list nil)
		(player-id nil))

  (defun get-agent (id)
	 (gethash id agent-table))

  (defun get-player ()
	 (get-agent player-id))

  (defun get-agent-table ()
	 agent-table)

  (defun make-agent (kind)
	 (case kind
		((:player) (make-instance 'agent
										  :location (point (coord 1 0.0) (coord 1 0.0))
										  :collision t
										  :damage 3
										  :health 20
										  :max-health 20
										  :regen 0.33
										  :defense 1
										  :effects nil
										  :kind kind))
		((:item) (make-instance 'agent
										:location (gen-enemy-loc)
										:collision nil
										:damage 2
										:health 100
										:max-health 100
										:regen 2.0
										:defense 5
										:effects nil
										:kind kind))
		((:enemy t) (make-instance 'agent
											:location (gen-enemy-loc)
											:collision t
											:damage 1
											:health 5
											:max-health 5
											:regen .34
											:defense 0
											:effects nil
											:kind kind))))

  (defun initialize-agent (kind)
	 (let* ((agent (make-agent kind))
			  (id (agent-id agent)))
		(setf (gethash id agent-table) agent)
		(push id move-ticks-list)
		(if (eq kind :player)
		  (setf player-id id))))

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

  (defun compute-visible-agents (visible-agents-table origin &optional (visible-tiles-table (full-compute-visible-tiles (make-hash-table :test 'equal) origin)))
	 (let ((visible-tiles (hash-keys visible-tiles-table)))
		(dolist (agent (hash-values agent-table))
		  (let* ((agent-loc (agent-location agent))
					(test-loc (pair (abs-of (y-of agent-loc)) (abs-of (x-of agent-loc)))))
			 (if (member test-loc visible-tiles :test 'equal)
				(setf (gethash agent-loc visible-agents-table) agent))))))

  (defun add-ticks (agent num)
	 (setf (agent-ticks agent) (+ num (agent-ticks agent))))

  (defun pick-up (agent)
	 (let ((lst nil))
		(dolist (id (hash-values (agent-visible-agents agent)))
		  (if (equal (agent-location agent) (agent-location (get-agent id)))
			 (push id lst)))
		(setf lst (reverse lst))
		(dolist (item (mapcar #'get-agent lst))
		  (push item (agent-inventory agent))
		  (setf (agent-location item) (agent-id agent)))

		))

  (defun exec-key (key agent)
	 (case key
		((9 :9 :kp-9) (move agent 9))
		((8 :8 :kp-8) (move agent 8))
		((7 :7 :kp-7) (move agent 7))
		((6 :6 :kp-6) (move agent 6))
		((5 :5 :kp-5) (move agent 5))
		((4 :4 :kp-4) (move agent 4))
		((3 :3 :kp-3) (move agent 3))
		((2 :2 :kp-2) (move agent 2))
		((1 :1 :kp-1) (move agent 1))
		((:comma) (pick-up agent))
		(t (format *my-error-log* "Key not found~%"))))

  (defun insert-before (list position new-element)
	 (if (zerop position)
		(push new-element list)
		(push new-element (cdr (nthcdr (1- position) list))))
	 list)

  (defun print-ticks-list ()
	 (dolist (agent-id move-ticks-list) (pprint (list agent-id (agent-ticks (get-agent agent-id)))))
	 (format t "~%"))

  (defun sort-ticks-list ()
	 (let ((moved-agent-id (pop move-ticks-list))
			 (list-place 0))
		(loop for agent-id in move-ticks-list
				until (< (agent-ticks (get-agent moved-agent-id))
								(agent-ticks (get-agent agent-id)))
				do (incf list-place))
		(setf move-ticks-list (insert-before move-ticks-list list-place moved-agent-id))))

  (defun enemy-ai (agent)
	 (let ((command (1+ (random 9))))
		(if (eq (agent-kind agent) :item)
		  (setf command 5))
		command))

  (defun compute-vision (agent)
	 (setf (agent-visible-tiles agent) (make-hash-table :test 'equal))
	 (full-compute-visible-tiles (agent-visible-tiles agent) (agent-location agent))

	 (setf (agent-visible-agents agent) (make-hash-table :test 'equal))
	 (compute-visible-agents (agent-visible-agents agent) (agent-location agent) (agent-visible-tiles agent)))

  (defun update-agent (agent)
	 (compute-vision agent)
	 (let ((command (enemy-ai agent)))
		command))

  (defun update-agents (commands)
	 (let ((tick-number (agent-ticks (get-agent (first move-ticks-list)))))
		(dolist (id move-ticks-list)
		  (decf (agent-ticks (get-agent id)) tick-number)))
	 (loop
		do (let* ((agent-id (first move-ticks-list))
					 (agent (get-agent agent-id))
					 (command (update-agent agent)))
			  (if (eq (agent-kind agent) :player)
				 (if (eq commands nil)
					(return)
					(setf command (pop commands))))
			  (if (exec-key command agent)
				 (sort-ticks-list)))))

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
		(if (or (not (agent-solid-p agent))
					(check-empty-p target-loc))
		  (progn (setf (agent-location agent) target-loc)
					(setf (agent-ticks agent) (+ (agent-ticks agent) 600)))
		  nil)))

 ) 
