(require 'asdf)
(asdf:operate 'asdf:load-op :cl-mongo)
(use-package :cl-mongo)

(defparameter *id-mapping* (make-hash-table :test 'equal))

(db.use "test")

(let ((counter 0))
  (defun get-next-id () (incf counter)))

(defun import-person (pers)
  (let ((doc (make-document))
		(id (get-next-id)))
	(setf (gethash (getf pers :id) *id-mapping*) id); (get-element :_id doc))
	(add-element "id" id doc)
	(add-element "name" (getf pers :name) doc)
	(add-element "middle-name" (getf pers :middle-name) doc)
	(add-element "surname" (getf pers :surname) doc)
	(add-element "date" (getf pers :date) doc)
	(add-element "death-date" (getf pers :death-date) doc)
	(add-element "photo" (getf pers :photo) doc)
	(add-element "date-unknown" (getf pers :date-unknown) doc)
	(add-element "sex" (if (eql 'male (getf pers :sex))
						 "male"
						 "female")
				 doc)
	(add-element "owner" "fat" doc)
	(db.save "people" doc)))

(defun get-oid (id)
  (if id
	(cl-mongo::make-bson-oid :oid (gethash id *id-mapping*))
	nil))

(defun get-real-id (id)
  (if id
	(gethash id *id-mapping*)
	nil))

(defun import-marriage (marr)
  (let ((doc (make-document)))
	(add-element "id" (get-next-id) doc)
	(add-element "surname" (getf marr :surname) doc)
	(add-element "date" (getf marr :date) doc)
	(add-element "date-unknown" (getf marr :date-unknown) doc)
	(add-element "photo" (getf marr :photo) doc)
	(add-element "man" (gethash (getf marr :man) *id-mapping*) doc);(get-oid (getf marr :man)) doc)
	(add-element "wife" (gethash (getf marr :wife) *id-mapping*) doc);(get-oid (getf marr :wife)) doc)
	(add-element "children" (loop for child in (getf marr :children)
								  collect (gethash child *id-mapping*)) doc)
	(add-element "owner" "fat" doc)
	(db.save "weddings" doc)))

(defun set-counter (val)
  (let ((doc (make-document)))
	(add-element "name" "uniq" doc)
	(add-element "c" val doc)
	(db.save "counters" doc)))

(defun import-file (file)
  (let ((data (eval (read file))))
	(mapcar #'import-person (getf data :people))
	(mapcar #'import-marriage (getf data :weddings))
	(set-counter (get-next-id))))

(with-open-file (file "zao.dat" :direction :input)
  (import-file file))

(quit)


