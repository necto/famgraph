
(in-package #:kin-package)

(defparameter *year-per-px* 0.08)
(defparameter *start-year* 1750)
(defparameter *origin* (make-vect 40 0))

(defparameter *p-size* (make-vect (+ 200 5) (+ 100 5)))
(defparameter *wedding-size* (make-vect (+ 140 5) (+ 80 5)))

(defun age-to-height (age &key (absolute nil))
  (round (/ (if absolute (- age *start-year*) age) *year-per-px*)))

(defun safe-age-to-height (age &key (absolute nil))
  (if age
	(age-to-height age :absolute absolute)))

(defun height-to-age (height &key (absolute nil))
  (round
	(+ (* height *year-per-px*)
	   (if absolute *start-year* 0))))

(defparameter *date-adjust-step-wedding* 20)
(defparameter *date-adjust-step-child* 7)

(defun node-full-height (node)
  (ctypecase (node-data node)
	(marriage (vec-y (node-size node)))
	(person (max (age-to-height (p-age (node-data node)))
				 (vec-y (node-size node))))))

(defun init-tree (data)
  (iter (for n in data)
		(collect
		  (make-node
			:pos (make-vect 0 0)
			:size (ctypecase n
					(person *p-size*)
					(marriage *wedding-size*))
			:preds nil
			:succs nil
			:data n))))

(defun propose-node-date (node date)
  (setf (item-date-unknown (node-data node)) t)
  (setf (item-date (node-data node)) date))

(defun get-usual-date-step (from)
  (ctypecase (node-data from)
	(marriage *date-adjust-step-child*)
	(person *date-adjust-step-wedding*)))


(defun suggest-neighbour-adges (node)
  (let ((date (item-date (node-data node))))
	(when date
	  (iter (for succ in (node-succs node))
			(when (not (item-date (node-data succ)))
			  (propose-node-date succ
				(+ date (get-usual-date-step node)))
			  (suggest-neighbour-adges succ)))
	  (iter (for pred in (node-preds node))
			(when (not (item-date (node-data pred)))
			  (propose-node-date pred
				(- date (get-usual-date-step pred)))
			  (suggest-neighbour-adges pred))))))

(defun suggest-adges (nodes)
  (iter (for n in nodes)
		(suggest-neighbour-adges n))
  (iter (for n in nodes)
		(when (null (item-date (node-data n)))
		  (propose-node-date n *start-year*)
		  (suggest-neighbour-adges n))))

(defun arrange-vertically (nodes)
  (iter (for n in nodes)
		(setf (vec-y (node-pos n))
			  (age-to-height (item-date (node-data n))
							 :absolute t))))

(defun fill-edges (nodes weddings)
  (labels ((get-node (id)
			 (let ((ret (find-if #'(lambda (node)
									 (equalp (id (node-data node))
											 id))
								 nodes)))
			   (assert ret (id) "wrong id")
			   ret))
		   (add-edge (start-id finish-id)
			 (let ((start (get-node start-id))
				   (finish (get-node finish-id)))
			   (pushnew finish (node-succs start))
			   (pushnew start (node-preds finish)))))
	(iter (for wed in weddings)
		  (when (m-man wed)
			(add-edge (m-man wed) (id wed)))
		  (when (m-wife wed)
		    (add-edge (m-wife wed) (id wed)))
		  (iter (for child in (m-children wed))
				(add-edge (id wed) child)))))

#+nil(
(defun compare-nodes (a b)
  (ctypecase a
	(marriage (ctypecase b
				(marriage (and (m-date a)
							   (or (not (m-date b))
								   (< (m-date a) (m-date b)))))
				(person  (not (or (equal (id b) (m-man a))
								 (equal (id b) (m-wife a)))))))
;								 (find (id b) (m-children a) :test #'equal)))));(if (not (equal (id b) (m-man a))) t))))
	(person (ctypecase b
			  (marriage (not (compare-nodes b a)))
			  (person (cond ((and (equal (p-sex a) 'male)
							      (equal (p-sex b) 'female)) t)
							((and (equal (p-sex a) (p-sex b))
								  (p-date a)
								  (or (not (p-date b))
								      (< (p-date a) (p-date b)))) t)
							(t nil)))))))
)

(defun compare-nodes (a b)
  (let ((date-a (item-date (node-data a)))
		(date-b (item-date (node-data b))))
	(cond ((null date-a) date-b)
		  ((null date-b) (not date-b))
		  (t (< date-a date-b)))))

(defun build-tree (owner)
  (let ((people (get-people *storage* :owner owner))
		(weddings (get-weddings *storage* :owner owner)))
	(let ((nodes (sort (init-tree (append people weddings)); ))
					   #'compare-nodes)))
	  (fill-edges nodes weddings)
	  (suggest-adges nodes)
	  (arrange-vertically nodes)
	  (sort nodes #'compare-nodes))))

(defun place-nodes (nodes &optional (origin *origin*))
;  (arrange-nodes-tight nodes)
;  (time (arrange-tree-broad (first nodes)))
  (arrange-forest-broad nodes)
  (to-positives nodes)
  (move-tree nodes origin)
  nodes)

(defun get-tree-size (nodes)
  (iter (for n in nodes)
		(maximize (vec-x (node-right-down n)) into w)
		(maximize (+ (vec-y (node-pos n)) (node-full-height n)) into h)
		(finally (return (make-vect w h)))))


