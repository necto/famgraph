(in-package #:kin-package)

(defstruct (vec (:constructor make-vect (x y)))
  		   (x 0 :type integer)
		   (y 0 :type integer))

(defun vec+ (a b)
  (make-vect (+ (vec-x a) (vec-x b))
			 (+ (vec-y a) (vec-y b))))

(defun vec- (a b)
  (make-vect (- (vec-x a) (vec-x b))
			 (- (vec-y a) (vec-y b))))

(defun vec/ (v k)
  (make-vect (round (/ (vec-x v) k))
			 (round (/ (vec-y v) k))))

(defstruct (node (:print-object print-node))
  pos
  size
  preds
  succs
  marks
  data)

(defun mark-node (n mark)
  (pushnew mark (node-marks n)))

(defun node-environ (node)
  (append (node-preds node) (node-succs node)))

(defun unmark-node (n mark)
  (setf (node-marks n) (remove mark (node-marks n))))

(defun node-marked (n mark)
  (find mark (node-marks n)))

(defun clear-marks (n)
  (setf (node-marks n) nil))

(defun node-right-down (node)
  (vec+ (node-pos node) (node-size node)))

(defun node-centre (node)
  (vec+ (node-pos node) (vec/ (node-size node) 2)))

;; TODO: generalize macro
(defmacro extract-unmarked (marker list)
  `(iter (for node in ,list)
		 (if (not (node-marked node ,marker))
		   (collect node))))

(defun print-node (node stream)
  (flet ((get-ids (nodes)
		   (iter (for n in nodes)
				 (collect (item-id (node-data n))))))
	(format stream 
	  		"( pos:~a size:~a~% marks:~a preds:~a succs:~a data:~a )"
			(node-pos node)
			(node-size node)
			(node-marks node)
			(get-ids (node-preds node))
			(get-ids (node-succs node))
			(node-data node))))


(defstruct edge
  start finish color)
