
(defpackage #:tree (:use #:cl #:iter)
  (:export :node
		   :arrange-nodes-tight
		   :arrange-tree-broad
		   :arrange-forest-broad
		   :to-positives
		   :move-tree

		   :node
		   :make-node
		   :node-pos
		   :node-right-down
		   :node-size
		   :node-preds
		   :node-succs
		   :node-marks
		   :node-data

		   :edge
		   :build-edges
		   :edge-start
		   :edge-finish
		   :edge-color

		   :vec
		   :vec-x
		   :vec-y
		   :make-vect))

(in-package :tree)

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
  (flet ((get-datas (nodes)
		   (iter (for n in nodes)
				 (collect (node-data n)))))
	(format stream 
	  		"( pos:~a size:~a~% marks:~a preds:~a succs:~a data:~a )"
			(node-pos node)
			(node-size node)
			(node-marks node)
			(get-datas (node-preds node))
			(get-datas (node-succs node))
			(node-data node))))


(defstruct edge
  start finish color)
