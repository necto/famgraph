
(in-package #:kin-package)

(defparameter *now* 2012)

(defstruct item
  id
  (date-unknown nil)
  date)

(defstruct (person (:include item))
	name
	middle-name
	surname
	sex
	photo
	death-date
	death-date-unknown)

(defun person-age (pers)
  (- (if (person-death-date pers)
	   (person-death-date pers)
	   *now*)
	 (person-date pers)))

(defun safe-person-age (pers)
  (if (person-date pers)
	(person-age pers)))

(defstruct (marriage (:include item))
  man
  wife
  surname
  photo
  children)

(export '(make-marriage make-person))

(defgeneric get-people (storage))
(defgeneric get-weddings (storage))
(defgeneric get-person (id storage))
(defgeneric get-wedding (id storage))
(defgeneric alter-person (pers storage))
(defgeneric insert-marriage (marr storage))
(defgeneric alter-marriage (marr storage))
(defgeneric insert-person (pers storage))
(defgeneric get-uniq-number (storage))

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

(defclass database ()
  ((people :reader people)
   (weddings :reader weddings)
   (counters :reader counters)))

(defmethod initialize-instance :after ((db database) &key)
  (let ((base (make-instance 'mongo:database :name "test")))
	(setf (slot-value db 'people)
		  (mongo:collection base "people"))
	(setf (slot-value db 'weddings)
		  (mongo:collection base "weddings"))
	(setf (slot-value db 'counters)
		  (mongo:collection base "counters"))))

(defun doc-to-person (pers)
  (make-person :id (gethash "id" pers)
			   :name (gethash "name" pers)
			   :middle-name (gethash "middle-name" pers)
			   :surname (gethash "surname" pers)
			   :sex (if (equal "male" (gethash "sex" pers))
					  'male
					  'female)
			   :photo (gethash "photo" pers)
			   :date (gethash "date" pers)
			   :date-unknown (gethash "date-unknown" pers)
			   :death-date (gethash "death-date" pers)
			   :death-date-unknown (gethash "death-date-unknown" pers)))

(defun doc-to-wedding (wedd)
  (make-marriage :id (gethash "id" wedd)
				 :surname (gethash "surname" wedd)
				 :man (gethash "man" wedd)
				 :wife (gethash "wife" wedd)
				 :children (gethash "children" wedd)
				 :date (gethash "date" wedd)
				 :photo (gethash "photo" wedd)
				 :date-unknown (gethash "date-unknown" wedd)))

(defmethod get-people ((storage database))
  (iter (for pers in (mongo:find-list (people storage) :query (son)))
		(collect (doc-to-person pers))))

(defmethod get-weddings ((storage database))
  (iter (for wedd in (mongo:find-list (weddings storage) :query (son)))
		(collect (doc-to-wedding wedd))))

(defmethod get-person (id (storage database))
  (when id
	(doc-to-person (mongo:find-one (people storage) (son "id" id)))))

(defmethod get-wedding (id (storage database))
  (doc-to-wedding (mongo:find-one (weddings storage) (son "id" id))))

(defun person-to-doc (pers)
  (let ((doc (make-hash-table :test 'equal)))
	(setf (gethash "id" doc) (person-id pers))
	(setf (gethash "name" doc) (person-name pers))
	(setf (gethash "middle-name" doc)  (person-middle-name pers))
	(setf (gethash "surname" doc) (person-surname pers))
	(setf (gethash "sex" doc) (if (eq 'male (person-sex pers))
							    "male"
							    "female"))
	(setf (gethash "date" doc) (person-date pers))
	(setf (gethash "death-date" doc) (person-death-date pers))
	(setf (gethash "date-unknown" doc) (person-date-unknown pers))
	(setf (gethash "death-date-unknown" doc)
		  (person-death-date-unknown pers))
	(setf (gethash  "photo" doc) (person-photo pers))
	doc))

(defmethod alter-person (pers (storage database))
  (mongo:update-op (people storage)
				   (son "id" (person-id pers))
				   (person-to-doc pers)))

(defmethod insert-person (pers (storage database))
  (mongo:insert-op (people storage) (person-to-doc pers)))

(defun marriage-to-doc (marr)
  (let ((doc (make-hash-table :test 'equal)))
	(setf (gethash "id" doc) (marriage-id marr))
	(setf (gethash "surname" doc) (marriage-surname marr))
	(setf (gethash "man" doc) (marriage-man marr))
	(setf (gethash "wife" doc) (marriage-wife marr))
	(setf (gethash "children" doc) (marriage-children marr))
	(setf (gethash "date-unknown" doc) (marriage-date-unknown marr))
	(setf (gethash "photo" doc) (marriage-photo marr))
	doc))

(defmethod alter-marriage (marr (storage database))
  (mongo:update-op (weddings storage)
				   (son "id" (marriage-id marr))
				   (marriage-to-doc marr)))

(defmethod insert-marriage (marr (storage database))
  (mongo:insert-op (weddings storage) (marriage-to-doc marr)))

(defmethod get-uniq-number ((storage database))
  (let ((counter (mongo:find-one (counters storage) (son "name" "uniq"))))
	(let ((val (gethash "c" counter)))
	  (incf (gethash "c" counter))
	  (mongo:update-op (counters storage) (son "name" "uniq") counter)
	  (floor val))))

;(defvar *storage* (make-instance 'file-storage :fname "zao.dat"))
(defvar *storage* (make-instance 'database))
;
