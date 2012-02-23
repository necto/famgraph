(in-package #:kin-package)

;; Note b > a !!
(defstruct (interval (:constructor make-int (a b))
					 (:conc-name int-))
  a b)

(defun move-int (int dx)
  (make-int (+ (int-a int) dx) (+ (int-b int) dx)))

(defun make-proper-int (a b)
  (if (> b a)
	(make-int a b)
	(make-int b a)))

(defun int-len (int)
  (- (int-b int) (int-a int)))

(defun int-intersects (one two)
  (and (< (int-a one) (int-b two))
	   (> (int-b one) (int-a two))))

(defun int-contain (int point)
  (and (< (int-a int) point)
	   (> (int-b int) point)))

(defun node-y-interval (node)
  (make-int (vec-y (node-pos node))
			(vec-y (node-right-down node))))

(defun node-x-interval (node)
  (make-int (vec-x (node-pos node))
			(vec-x (node-right-down node))))

(defun is-y-intersect (one two)
  (int-intersects (node-y-interval one)
				  (node-y-interval two)))

(defstruct box
  x-int
  y-int)

(defun move-box (box dr)
  (make-box :x-int (move-int (box-x-int box) (vec-x dr))
			:y-int (move-int (box-y-int box) (vec-y dr))))

(defun node-box (node)
  (let ((left-up (node-pos node))
		(right-down (node-right-down node)))
	(make-box :x-int (make-int (vec-x left-up) (vec-x right-down))
			  :y-int (make-int (vec-y left-up) (vec-y right-down)))))

(defun y-interfering (boxes box)
  (iter (for b in boxes)
		(if (int-intersects (box-y-int b) (box-y-int box))
		  (collect (box-x-int b)))))

(defun contemporaries (nodes node)
  (y-interfering (mapcar #'node-box (extract-unmarked :unplaced nodes)) (node-box node)))

(defun get-right-intervals (intervals border)
  (iter (for int on intervals)
		(if (> (int-b (car int)) border)
		  (leave int))))

(defun find-first-right-place (occupied size &optional (start 0))
  (let ((occupied (get-right-intervals occupied start)))
	(if (not occupied)
	  start
	  (if (int-intersects (first occupied)
						  (make-int start (+ start size)))
		(let ((next-chance (int-b (first occupied))))
		  (find-first-right-place occupied size next-chance))
	  start))))

(defun get-left-intervals (intervals border)
  (iter (for int on intervals)
		(if (< (int-a (car int)) border)
		  (leave int))))

(defun find-first-left-place (occupied size &optional (start 0))
  (let ((occupied (get-left-intervals occupied (+ start size))))
	(if (not occupied)
	  start
	  (if (int-intersects (first occupied)
						  (make-int start (+ start size)))
		(let ((next-chance (- (int-a (first occupied)) size)))
		  (find-first-left-place occupied size next-chance))
	  start))))

(defun find-place (occupied size &optional (start 0))
  (let ((right (find-first-right-place (sort (copy-list occupied) #'< :key #'int-a) size start))
		(left  (find-first-left-place (sort (copy-list occupied) #'> :key #'int-b) size start)))
	(if (> (- right start) (- start left))
	  left
	  right)))

(defun tree-left (nodes)
  (iter (for n in nodes)
		(minimize (vec-x (node-pos n)))))

(defun move-tree (nodes dr)
  (iter (for n in nodes)
		(setf (node-pos n)
			  (vec+ (node-pos n) dr)))
  nodes)

(defun to-positives (nodes)
  (move-tree nodes (make-vect (- (tree-left nodes)) 0)))

(defun middle-x (nodes)
  (let ((placed (extract-unmarked :unplaced nodes)))
	(if placed
	  (/ (iter (for n in placed)
			   (sum (vec-x (node-centre n))))
		 (length placed)))))

(defun get-relatives (node)
  (let ((relatives nil))
  	(iter (for pred in (node-preds node))
		  (push pred relatives)
		  (iter (for bro in (node-succs pred))
				(push bro relatives)))
	(iter (for succ in (node-succs node))
		  (push succ relatives)
		  (iter (for wife in (node-preds succ))
				(push wife relatives))) ;Something wrong here
	(remove-duplicates relatives)
	))

(defun desired-place (node)
  (round (- (let ((middle (middle-x (get-relatives node))))
			  (if middle
				middle
				(/ (vec-x (node-size node)) 2)))
			(/ (vec-x (node-size node)) 2))))

(defun arrange-nodes-tight (nodes)
  (let (unplaced milled)
	(iter (for n in nodes)
		  (mark-node n :unplaced))
	(iter (for node on (reverse nodes))
		  (let ((n (car node)))
			(if (and milled
					 (or (not (node-preds n))
						 (and (every #'(lambda (node) (node-marked node :unplaced))
									 (node-preds n)))))
			  (push n unplaced)
			  (progn
				(setf (vec-x (node-pos n))
					  (find-place (contemporaries milled n)
								  (vec-x (node-size n))
								  (desired-place n)))
				(push n milled)
				(unmark-node n :unplaced))
			  )))
	(iter (for n in  unplaced)
		  (setf (vec-x (node-pos n))
				(find-place (contemporaries milled n)
							(vec-x (node-size n))
							(desired-place n)))
		  (unmark-node n :unplaced)
		  (push n milled))))

(defun covering-box (boxes)
  (iter (for box in boxes)
		(maximize (int-b (box-x-int box)) into right)
		(maximize (int-b (box-y-int box)) into down)
		(minimize (int-a (box-x-int box)) into left)
		(minimize (int-a (box-y-int box)) into up)
		(finally (return (make-box :x-int (make-int left right)
								   :y-int (make-int up down))))))

(defmacro iter-environ ((i root source) &body body)
  `(iter (for ,i in (node-environ ,root))
		 (when (not (eq ,source ,i))
		   ,@body)))

(defun get-subtree-box (root source)
  (covering-box (append (list (node-box root))
						(iter-environ (node root source)
						  (collect (get-subtree-box node root))))))

(defun move-subtree (root dr source)
  (setf (node-pos root) (vec+ (node-pos root) dr))
  (iter-environ (node root source)
	(move-subtree node dr root)))

(defun arrange-subtree (root source)
  (let ((located (list (node-box root))))
	(iter-environ (env root source)
				  (arrange-subtree env root)
				  (let* ((box (get-subtree-box env root))
						 (centre (round (- (vec-x (node-centre root))
										   (vec-x (node-centre env))
										   (- (vec-x (node-pos env)) (int-a (box-x-int box)))))))
					(let ((dr (make-vect (- (find-place (y-interfering located box)
														(int-len (box-x-int box))
														centre)
											(int-a (box-x-int box)))
										 0)))
					  (move-subtree env dr root)
					  (push (move-box box dr) located))))))

(defun arrange-tree-broad (root)
  (arrange-subtree root nil))

(defun mark-subree (root mark &optional (source nil))
  (iter-environ (env root source)
				(mark-subree env mark root)
				(mark-node env mark)))

(defun arrange-forest-broad (nodes)
  (let ((roots nil))
	(iter (for n in nodes)
		  (when (not (node-marked n :accounted))
			(push n roots)
			(mark-subree n :accounted)))
	(arrange-tree-broad (make-node :pos (make-vect 0 0)
								   :size (make-vect 0 0)
								   :preds nil
								   :succs roots
								   :marks nil
								   :data nil))))

(defun build-edges (nodes)
  (iter outer (for s in nodes)
		(iter (for f in (node-succs s))
			  (in outer (collect (make-edge :start (node-centre s)
											:finish (node-centre f)))))))
