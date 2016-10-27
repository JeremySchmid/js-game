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
	  (visible-tiles :accessor agent-visible-tiles
							 :initform (make-hash-table :test 'equal))
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

	(defun check-empty-p (target-loc)
	  (and (floor-p (apply #'get-tile-value target-loc))
					(not (member target-loc (mapcar #'agent-location (list-hash-values agent-table)) :test 'equal))))

  (defun gen-enemy-loc ()
	 (let ((prospect (list (random 20) (random 20))))
		(if (check-empty-p prospect)
		  prospect
		  (gen-enemy-loc))))

  (defun make-agent (kind)
	 (case kind
		((:player) (make-instance 'agent
										  :location '(1 1)
										  :damage '(3 0 0)
										  :health '(20 7)
										  :max-health '(20 7)
										  :regen '(0.33 0.5)
										  :defense '(1 0 0)
										  :effects nil
										  :kind kind))
		((:object) (make-instance 'agent
										  :location #(1 1)
										  :damage '(0 0 0)
										  :health '(100 25)
										  :max-health '(100 25)
										  :regen '(2.0 1.0)
										  :defense '(2 1 1)
										  :effects nil
										  :kind kind))
		((:enemy t) (let ((location (gen-enemy-loc))) (make-instance 'agent
								:location location
								:damage '(1 0 0)
								:health '(5 2)
								:max-health '(5 2)
								:regen '(.34 0.17)
								:defense '(0 0 0)
								:effects nil
								:kind kind)))))
  
  (defun initialize-agent (kind)
	 (let* ((agent (make-agent kind))
			  (id (agent-id agent)))
		(push id agent-id-list)
		(setf (gethash id agent-table) agent)
		(if (eq kind :player)
		  (setf player-id id))))
  
  (defun update-agent (agent)
	 (setf (agent-visible-tiles agent) (make-hash-table :test 'equal))
	 (full-compute-visible-tiles (agent-visible-tiles agent) (agent-location agent)))

  (defun update-agents ()
	 (loop for id in agent-id-list
			 do (update-agent (gethash id agent-table))))
  
  (defun get-player ()
	 (gethash player-id agent-table))

  (defun move-direction (loc num)
	 (let ((vec (copy-list loc)))
		(case num
		  ((1 2 3) (setf (nth 0 vec) (- (nth 0 loc) 1)))
		  ((7 8 9) (setf (nth 0 vec) (+ (nth 0 loc) 1))))
		(case num
		  ((1 4 7) (setf (nth 1 vec) (- (nth 1 loc) 1)))
		  ((3 6 9) (setf (nth 1 vec) (+ (nth 1 loc) 1))))
		(values vec)))

  (defun move (agent num)
	 (let ((target-loc (move-direction (agent-location agent) num)))
		(if (check-empty-p target-loc)
		  (setf (agent-location agent) target-loc))))

  )
