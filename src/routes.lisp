
(in-package #:kin-package)

(restas:define-route main ("")
  (get-page))

(restas:define-route tree ("tree" :content-type "image/svg+xml")
  (draw-tree))

(defun parse-native-namestring (thing)
  #+sbcl (sb-ext:parse-native-namestring thing)
  #-sbcl (parse-namestring thing))

(restas:define-route route ("file/*path" :method :get)
  (let* ((relative-path (parse-native-namestring (format nil "~{~A~^/~}" path)))
         (path (merge-pathnames relative-path
                                "files/")))
	(cond
      ((find :up (pathname-directory relative-path))
	   	hunchentoot:+http-bad-request+)
	  ((fad:file-exists-p path) path)
      ((not (fad:file-exists-p path)) hunchentoot:+http-not-found+)
      (t path))))

(restas:define-route person-change-route ("person:(id)" :parse-vars (list :id #'parse-integer))
	(person-change (get-person id *storage*) :new nil))

(defun get-persons (list)
  (iter (for p in list)
		(collect (get-person p *storage*))))


(restas:define-route wedding-change-route
					 ("wedding:(id)" :parse-vars (list :id #'parse-integer))
  (let ((wedd (get-wedding id *storage*)))
	(wedding-change wedd (get-person (marriage-man wedd) *storage*)
						 (get-person (marriage-wife wedd) *storage*)
						 (get-persons (marriage-children wedd))
						 :new nil)))

(defun safe-parse-integer (str)
  (if (and str (not (equalp "" str)))
	(parse-integer str)))

;(let ((counter 0))
;  (defun gen-fname () (format nil "~a" (incf counter))))

(defun gen-fname () (format nil "~a" (get-universal-time)))

(defun handle-file (post-parameter subdir)
  (when (and post-parameter
			 (listp post-parameter))
	(destructuring-bind (path file-name content-type)
	  post-parameter
	  (declare (ignore content-type))
	  (setq file-name (format nil "~a" (gen-fname))) ; file-name))
	  (let ((new-path (merge-pathnames
						(make-pathname 
						  :name file-name
						  :type nil :directory `(:relative "files" ,@subdir)))))
;; strip directory info sent by Windows browsers
;	(when (search "Windows" (user-agent) :test 'char-equal)
;	  (setq file-name (cl-ppcre:regex-replace ".*\\\\" file-name "")))
	    (rename-file path (ensure-directories-exist new-path))
	    file-name))))

(defmacro post-param (name) `(hunchentoot:post-parameter ,name))

(restas:define-route upload-photo ("upload-photo" :method :post)
  (upload-finished (handle-file (post-param "file") '("image"))))

(restas:define-route alter-pers ("alter-person" :method :post)
  (let ((pers 
			(make-person 
			  :id (safe-parse-integer (post-param "id"))
			  :name (post-param "name")
			  :middle-name (post-param "middle-name")
			  :surname (post-param "surname")
			  :sex (if (equalp "male" (post-param "sex"))
					 'male
					 'female)
			  :date (safe-parse-integer (post-param "date"))
			  :death-date (safe-parse-integer (post-param "death"))
			  :date-unknown (not (null (post-param "date-unknown")))
			  :death-date-unknown (not (null (post-param "death-unknown")))
			  :photo (post-param "photo"))))
	(if (post-param "new")
	  (insert-person pers *storage*)
	  (alter-person pers *storage*))
	(refresh-tree)))

(defun get-children-list (param)
  (when (and param (not (string= "" param)))
	(with-input-from-string (is (post-param "children"))
	  (read is))))

(restas:define-route alter-marr ("alter-marriage" :method :post)
  (let ((wedd (make-marriage
				:id (safe-parse-integer (post-param "id"))
				:man (safe-parse-integer (post-param "man"))
				:wife (safe-parse-integer (post-param "wife"))
				:surname (post-param "surname")
				:photo (post-param "photo")
				:date (post-param "date")
				:date-unknown (post-param "date-unknown")
				:children (get-children-list (post-param "children")))))
	(if (post-param "new")
	  (insert-marriage wedd *storage*)
	  (alter-marriage wedd *storage*)))
  (refresh-tree))
	;(format nil "~a" wedd)))

;(restas:define-route nooa ("impossible:(id)" :method :get))

(restas:define-route get-card ("card:(id)" :method :get :content-type "image/svg+xml")
  (person-card (get-person (parse-integer id) *storage*)))

(restas:define-route new-person ("new-person" :method :get)
  (person-change (make-person :id (get-uniq-number *storage*)) :new t))

(restas:define-route new-wedding ("new-wedding" :method :get)
  (wedding-change (make-marriage :id (get-uniq-number *storage*))
				  nil nil nil :new t))

(restas:define-route new-items ("new-items")
  (draw-new-items))


