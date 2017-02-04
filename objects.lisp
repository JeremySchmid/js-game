
  (defclass object ()
	 ((location :accessor object-location
					:initarg :location)
	  (collision :accessor object-collision
					 :initarg :collision)
	  (damage :accessor object-damage
				 :initarg :damage)
	  (health :accessor object-health
				 :initarg :health)
	  (max-health :accessor object-max-health
					  :initarg :max-health)
	  (regen :accessor object-regen
				:initarg :regen)
	  (defense :accessor object-defense
				  :initarg :defense)
	  (id :accessor object-id
			:initform (gensym)
			:initarg :id)
	  (effects :accessor object-effects
				  :initarg :effects)
	  (kind :accessor object-kind
			  :initarg :kind)
	  (inventory :accessor object-inventory
					 :initarg nil
					 :initform :inventory)))

(defclass agent (object)
  
	  ((visible-objects :accessor agent-visible-objects
							:initform (make-hash-table :test 'equal))
	  (visible-tiles :accessor agent-visible-tiles
						  :initform (make-hash-table :test 'equal))
	  (ticks :accessor agent-ticks
				:initarg :ticks
				:initform 0)))

(let ((agent-table (make-hash-table :test 'equal))
		(item-table (make-hash-table :test 'equal))
		(move-ticks-list nil)
		(player-id nil))

  (defun get-object (id kind)
	 (gethash id (if (eq kind :item) item-table agent-table)))

  (defun get-player ()
	 (get-object player-id :player))

  (defun get-agent-table ()
	 agent-table)
  
  (defun get-item-table ()
	 item-table)

  (defun make-object (kind)
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
		((:item) (make-instance 'object
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

  (defun initialize-object (kind)
	 (let* ((object (make-object kind))
			  (id (object-id object))
			  (table (if (eq kind :item) item-table agent-table)))
		(setf (gethash id table) object)
		(if (not (eq kind :item))
		  (push id move-ticks-list))
		(if (eq kind :player)
		  (setf player-id id))))

  (defun object-solid-p (object)
	 (object-collision object))

  (defun check-empty-p (target-loc)
	 (and (floor-p (get-tile-value (abs-of (y-of target-loc)) (abs-of (x-of target-loc))))
			(not (member target-loc (mapcar #'object-location (hash-values agent-table)) :test 'equal))))

  (defun gen-enemy-loc ()
	 (let ((prospect (point (coord (random 20) 0.0) (coord (random 20) 0.0))))
		(if (check-empty-p prospect)
		  prospect
		  (gen-enemy-loc))))

  (defun compute-visible-objects (visible-objects-table origin &optional (visible-tiles-table (full-compute-visible-tiles (make-hash-table :test 'equal) origin)))
	 (let ((visible-tiles (hash-keys visible-tiles-table)))
		(dolist (object (append (hash-values agent-table) (hash-values item-table)))
		  (let* ((loc (object-location object))
					(kind (object-kind object))
					(test-loc (pair (abs-of (y-of loc)) (abs-of (x-of loc)))))
			 (if (member test-loc visible-tiles :test 'equal)
				(setf (gethash (cons loc kind) visible-objects-table) object))))))

  (defun add-ticks (agent num)
	 (setf (agent-ticks agent) (+ num (agent-ticks agent))))

  #|
  (defun pick-up (object)
	 (let ((lst nil))
		(dolist (id (hash-values (agent-visible-objects object)))
		  (if (equal (object-location object) (object-location (get-object id (object-kind object))))
			 (push id lst)))
		(setf lst (reverse lst))
		(dolist (item (mapcar #'get-object lst))
		  (push item (object-inventory object))
		  (setf (object-location item) (object-id object)))

		))
  |#

  (defun exec-key (key object)
	 (case key
		((9 :9 :kp-9) (move object 9))
		((8 :8 :kp-8) (move object 8))
		((7 :7 :kp-7) (move object 7))
		((6 :6 :kp-6) (move object 6))
		((5 :5 :kp-5) (move object 5))
		((4 :4 :kp-4) (move object 4))
		((3 :3 :kp-3) (move object 3))
		((2 :2 :kp-2) (move object 2))
		((1 :1 :kp-1) (move object 1))
		;((:comma) (pick-up object))
		(t (format *my-error-log* "Key not found~%"))))

  (defun insert-before (list position new-element)
	 (if (zerop position)
		(push new-element list)
		(push new-element (cdr (nthcdr (1- position) list))))
	 list)

  (defun print-ticks-list ()
	 (dolist (object-id move-ticks-list :agent) (pprint (list object-id (agent-ticks (get-object object-id :agent)))))
	 (format t "~%"))

  (defun sort-ticks-list ()
	 (let ((moved-object-id (pop move-ticks-list))
			 (list-place 0))
		(loop for object-id in move-ticks-list
				until (< (agent-ticks (get-object moved-object-id :agent))
								(agent-ticks (get-object object-id :agent)))
				do (incf list-place))
		(setf move-ticks-list (insert-before move-ticks-list list-place moved-object-id))))

  (defun enemy-ai (object)
	 (let ((command (1+ (random 9))))
		(if (eq (object-kind object) :item)
		  (setf command 5))
		command))

  (defun compute-vision (object)
	 (setf (agent-visible-tiles object) (make-hash-table :test 'equal))
	 (full-compute-visible-tiles (agent-visible-tiles object) (object-location object))

	 (setf (agent-visible-objects object) (make-hash-table :test 'equal))
	 (compute-visible-objects (agent-visible-objects object) (object-location object) (agent-visible-tiles object)))

  (defun update-agent (agent)
	 (compute-vision agent)
	 (let ((command (enemy-ai agent)))
		command))

  (defun update-agents (commands)
	 (let ((tick-number (agent-ticks (get-object (first move-ticks-list) :agent))))
		(dolist (id move-ticks-list)
		  (decf (agent-ticks (get-object id :agent)) tick-number)))
	 (loop
		do (let* ((agent-id (first move-ticks-list))
					 (agent (get-object agent-id :agent))
					 (command (update-agent agent)))
			  (if (eq (object-kind agent) :player)
				 (if (eq commands nil)
					(return)
					(setf command (car (pop commands)))))
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

  (defun move (object num)
	 (let ((target-loc (move-direction (object-location object) num)))
		(if (or (not (object-solid-p object))
					(check-empty-p target-loc))
		  (progn (setf (object-location object) target-loc)
					(setf (agent-ticks object) (+ (agent-ticks object) 600)))
		  nil)))

 ) 
