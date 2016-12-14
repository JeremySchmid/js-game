
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
				:initform 0)))

(let ((agent-table (make-hash-table :test 'equal))
		(move-ticks-list nil)
		(player-id nil))

  (defun get-player ()
	 (gethash player-id agent-table))

  (defun get-agent-table ()
	 agent-table)

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
		(push (list id (agent-ticks agent)) move-ticks-list)
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

  (defun exec-key (key agent)
	 (case key
		((:9 :kp-9) (move agent 9) (add-ticks agent 600))
		((:8 :kp-8) (move agent 8) (add-ticks agent 600))
		((:7 :kp-7) (move agent 7) (add-ticks agent 600))
		((:6 :kp-6) (move agent 6) (add-ticks agent 600))
		((:5 :kp-5) (move agent 5) (add-ticks agent 600))
		((:4 :kp-4) (move agent 4) (add-ticks agent 600))
		((:3 :kp-3) (move agent 3) (add-ticks agent 600))
		((:2 :kp-2) (move agent 2) (add-ticks agent 600))
		((:1 :kp-1) (move agent 1) (add-ticks agent 600))
		(t (format *my-error-log* "Key not found~%"))))

  (defun enemy-ai (agent)
	 (move agent (1+ (random 9)))
	 (add-ticks agent 600))

  (defun update-agent (key agent)
	 (setf (agent-visible-tiles agent) (make-hash-table :test 'equal))
	 (full-compute-visible-tiles (agent-visible-tiles agent) (agent-location agent))
	 (setf (agent-visible-agents agent) (make-hash-table :test 'equal))
	 (compute-visible-agents (agent-visible-agents agent) (agent-location agent) (agent-visible-tiles agent))
	 (if (eq (agent-kind agent) :player)
		(exec-key key agent)
		(enemy-ai agent)))

  (defun insert-before (lst index newelt)
	 (let ((partial-list (nthcdr index lst)))
		(push newelt partial-list)
		lst))

  (defun sort-ticks-list ()
	 (let ((moved-agent (pop move-ticks-list))
			 (list-place 0))
		(loop for agent in move-ticks-list
				until (< (second (first move-ticks-list)) (second moved-agent))
				do (incf list-place))
		(setf move-ticks-list (insert-before move-ticks-list list-place moved-agent))))

  (defun update-agents (commands)
	 (let ((top-agent (gethash (first (first move-ticks-list)) agent-table)))
		(loop for key in commands
				do (loop until (eq (agent-kind top-agent) :player)
							do (update-agent key top-agent)
							(setf (second (first move-ticks-list)) (agent-ticks top-agent))
							(sort-ticks-list)
							(setf top-agent (gethash (first (first move-ticks-list)) agent-table)))
				(update-agent key top-agent)
				(setf (second (first move-ticks-list)) (agent-ticks top-agent))
				(sort-ticks-list)
				(setf top-agent (gethash (first (first move-ticks-list)) agent-table)))))

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
		  (progn (setf (agent-location agent) target-loc)
					(setf (agent-ticks agent) (- (agent-ticks agent) 60))))))

 ) 
