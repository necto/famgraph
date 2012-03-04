
(in-package #:kin-package)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :percl))

(defparameter *now* 2012)


(defclass item (identifable)
  ((date-unknown :initform nil :accessor item-date-unknown)
   (date :accessor item-date :initform nil )))

(defclass person (item)
	((name					:accessor p-name :initform nil )
	 (middle-name			:accessor p-middle-name :initform nil)
	 (surname				:accessor p-surname :initform nil )
	 (sex					:accessor p-sex :initform nil )
	 (photo					:accessor p-photo :initform nil )
	 (death-date			:accessor p-death-date :initform nil)
	 (death-date-unknown	:accessor p-death-date-unknown :initform nil)))

(defun p-age (pers)
  (- (if (p-death-date pers)
	   (p-death-date pers)
	   *now*)
	 (item-date pers)))

(defun safe-p-age (pers)
  (if (item-date pers)
	(p-age pers)))

(defclass marriage (item)
  ((man			:accessor m-man :initform nil )
   (wife		:accessor m-wife :initform nil )
   (surname		:accessor m-surname :initform nil )
   (photo		:accessor m-photo :initform nil )
   (children	:accessor m-children :initform nil )))

(defgeneric get-people (storage))
(defgeneric get-weddings (storage))
(defgeneric get-person (id storage))
(defgeneric get-wedding (id storage))
(defgeneric alter-person (pers storage))
(defgeneric alter-marriage (marr storage))

#(
(defclass file-storage ()
  ((filename 
	 
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

;(print (macroexpand-1 '
(generate-methods person ('people database)
				  (('date "date" :type integer)
				   ('date-unknown "date-unknown" :type boolean)
				   ('photo "photo")
				   ('name "name") ('middle-name "middle-name")
				   ('surname "surname")
				   ('sex "sex" :set (('male "male") ('female "female")))
				   ('death-date "death-date" :type integer)
				   ('death-date-unknown "death-date-unknown"
					:type boolean)))

(generate-methods marriage ('weddings database)
				  (('date "date" :type integer)
				   ('date-unknown "date-unknown" :type boolean)
				   ('photo "photo") ('surname "surname")
				   ('man "man" :type integer) ('wife "wife" :type integer)
				   ('children "children" :type list)))

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
