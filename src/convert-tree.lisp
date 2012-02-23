
(in-package #:kin-package)

(defparameter *year-per-px* 0.08)
(defparameter *start-year* 1750)
(defparameter *origin* (make-vect 40 0))

(defparameter *person-size* (make-vect (+ 200 5) (+ 100 5)))
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
	(person (max (age-to-height (person-age (node-data node)))
				 (vec-y (node-size node))))))

(defun init-tree (data)
  (iter (for n in data)
		(collect
		  (make-node
			:pos (make-vect 0 0)
			:size (ctypecase n
					(person *person-size*)
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


(defun suggest-neibour-adges (node)
  (let ((date (item-date (node-data node))))
	(when date
	  (iter (for succ in (node-succs node))
			(when (not (item-date (node-data succ)))
			  (propose-node-date succ
				(+ date (get-usual-date-step node)))
			  (suggest-neibour-adges succ)))
	  (iter (for pred in (node-preds node))
			(when (not (item-date (node-data pred)))
			  (propose-node-date pred
				(- date (get-usual-date-step pred)))
			  (suggest-neibour-adges pred))))))

(defun suggest-adges (nodes)
  (iter (for n in nodes)
		(suggest-neibour-adges n))
  (iter (for n in nodes)
		(when (null (item-date (node-data n)))
		  (propose-node-date n *start-year*)
		  (suggest-neibour-adges n))))

(defun arrange-vertically (nodes)
  (iter (for n in nodes)
		(setf (vec-y (node-pos n))
			  (age-to-height (item-date (node-data n))
							 :absolute t))))

(defun fill-edges (nodes weddings)
  (labels ((get-node (id)
		     (find-if #'(lambda (node) (equalp (item-id (node-data node)) id)) nodes))
		   (add-edge (start-id finish-id)
			 (let ((start (get-node start-id))
				   (finish (get-node finish-id)))
			   (pushnew finish (node-succs start))
			   (pushnew start (node-preds finish)))))
	(iter (for wed in weddings)
		  (when (marriage-man wed)
			(add-edge (marriage-man wed) (item-id wed)))
		  (when (marriage-wife wed)
		    (add-edge (marriage-wife wed) (item-id wed)))
		  (iter (for child in (marriage-children wed))
				(add-edge (marriage-id wed) child)))))

#+nil(
(defun compare-nodes (a b)
  (ctypecase a
	(marriage (ctypecase b
				(marriage (and (marriage-date a)
							   (or (not (marriage-date b))
								   (< (marriage-date a) (marriage-date b)))))
				(person  (not (or (equal (item-id b) (marriage-man a))
								 (equal (item-id b) (marriage-wife a)))))))
;								 (find (item-id b) (marriage-children a) :test #'equal)))));(if (not (equal (item-id b) (marriage-man a))) t))))
	(person (ctypecase b
			  (marriage (not (compare-nodes b a)))
			  (person (cond ((and (equal (person-sex a) 'male)
							      (equal (person-sex b) 'female)) t)
							((and (equal (person-sex a) (person-sex b))
								  (person-date a)
								  (or (not (person-date b))
								      (< (person-date a) (person-date b)))) t)
							(t nil)))))))
)

(defun compare-nodes (a b)
  (let ((date-a (item-date (node-data a)))
		(date-b (item-date (node-data b))))
	(cond ((null date-a) date-b)
		  ((null date-b) (not date-b))
		  (t (< date-a date-b)))))

(defun build-tree ()
  (let ((nodes (sort (init-tree
								  (append (get-people *storage*)
										  (get-weddings *storage*))); ))
								#'compare-nodes)))
	(fill-edges nodes (get-weddings *storage*))
	(suggest-adges nodes)
	(arrange-vertically nodes)
	(sort nodes #'compare-nodes)))

(defun place-nodes (nodes &optional (origin *origin*))
;  (arrange-nodes-tight nodes)
;  (time (arrange-tree-broad (fourth nodes)))
  (arrange-forest-broad nodes)
  (to-positives nodes)
  (move-tree nodes origin)
  nodes)

(defun get-tree-size (nodes)
  (iter (for n in nodes)
		(maximize (vec-x (node-right-down n)) into w)
		(maximize (+ (vec-y (node-pos n)) (node-full-height n)) into h)
		(finally (return (make-vect w h)))))

