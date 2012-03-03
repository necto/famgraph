
(in-package #:kin-package)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :percl))

(defparameter *now* 2012)


(defclass item (identifable)
  ((date-unknown :initform nil :accessor item-date-unknown :initarg :date-unknown)
   (date :accessor item-date :initform nil :initarg :date)))

(defclass person (item)
	((name					:accessor person-name :initform nil :initarg :name)
	 (middle-name			:accessor person-middle-name :initform nil :initarg :middle-name)
	 (surname				:accessor person-surname :initform nil :initarg :surname)
	 (sex					:accessor person-sex :initform nil :initarg :sex)
	 (photo					:accessor person-photo :initform nil :initarg :photo)
	 (death-date			:accessor person-death-date :initform nil :initarg :death-date)
	 (death-date-unknown	:accessor person-death-date-unknown :initform nil :initarg :death-date-unknown)))

(defun person-age (pers)
  (- (if (person-death-date pers)
	   (person-death-date pers)
	   *now*)
	 (item-date pers)))

(defun safe-person-age (pers)
  (if (item-date pers)
	(person-age pers)))

(defclass marriage (item)
  ((man			:accessor marriage-man :initform nil :initarg :man)
   (wife		:accessor marriage-wife :initform nil :initarg :wife)
   (surname		:accessor marriage-surname :initform nil :initarg :surname)
   (photo		:accessor marriage-photo :initform nil :initarg :photo)
   (children	:accessor marriage-children :initform nil :initarg :children)))

(defgeneric get-people (storage))
(defgeneric get-weddings (storage))
(defgeneric get-person (id storage))
(defgeneric get-wedding (id storage))
(defgeneric alter-person (pers storage))
(defgeneric alter-marriage (marr storage))

#(
(defclass file-storage ()
  ((filename 
	 :initarg :fname
	 :accessor fname)))

(defun generate-people (props)
  (mapcar #'(lambda (prop) (apply #'make-person prop)) props))

(defun generate-marriages (props)
  (mapcar #'(lambda (prop) (apply #'make-marriage prop)) props))

(defmethod get-people ((storage file-storage))
  (with-open-file (file (fname storage))
	(generate-people (getf (eval (read file)) :people))))

(defmethod get-weddings ((storage file-storage))
  (with-open-file (file (fname storage))
	(generate-marriages (getf (eval (read file)) :weddings))))
)

(defclass database (database-base)
  ((people :reader people)
   (weddings :reader weddings)))

(defmethod initialize-instance ((db database) &key)
  (let ((base (make-instance 'mongo:database :name "test")))
	(setf (slot-value db 'db) base)
	(setf (slot-value db 'people)
		  (mongo:collection base "people"))
	(setf (slot-value db 'weddings)
		  (mongo:collection base "weddings"))))

;(print (macroexpand-1
(generate-methods person ('people database)
				  (('date "date" :type integer) ('date-unknown "date-unknown")
				   ('photo "photo")
				   ('name "name") ('middle-name "middle-name")
				   ('surname "surname")
				   ('sex "sex" :set (('male "male") ('female "female")))
				   ('death-date "death-date")
				   ('death-date-unknown "death-date-unknown")))

(generate-methods marriage ('weddings database)
				  (('date "date" :type integer) ('date-unknown "date-unknown")
				   ('photo "photo") ('surname "surname")
				   ('man "man") ('wife "wife")
				   ('children "children")))

(defmethod get-people ((storage database))
  (load-all-instances 'person storage))
(defmethod get-weddings ((storage database))
  (load-all-instances 'marriage storage))
(defmethod get-person (id (storage database))
  (load-inst 'person id storage))
(defmethod get-wedding (id (storage database))
  (load-inst 'marriage id storage))
(defmethod alter-person (pers (storage database))
  (store-inst pers storage))
(defmethod alter-marriage (marr (storage database))
  (store-inst marr storage))

;(defvar *storage* (make-instance 'file-storage :fname "zao.dat"))
(defvar *storage* (make-instance 'database))
;
