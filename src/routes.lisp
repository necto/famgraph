
(in-package #:kin-package)

(restas:define-route main ("edit:(owner)")
  (get-page owner (search "Firefox" (hunchentoot:user-agent))))

(restas:define-route tree ("tree:(owner)" :content-type "image/svg+xml")
  (draw-tree :owner owner))

(restas:define-route view-tree ("view-tree:(owner)" :content-type "image/svg+xml")
  (draw-view-tree owner))

(defun parse-native-namestring (thing)
  #+sbcl (sb-ext:parse-native-namestring thing)
  #-sbcl (parse-namestring thing))

(restas:define-route route ("file/*path" :method :get)
  (let* ((relative-path (parse-native-namestring
						  (format nil "~{~A~^/~}" path)))
         (path (merge-pathnames relative-path
                                "files/")))
	(cond
      ((find :up (pathname-directory relative-path))
	   	hunchentoot:+http-bad-request+)
	  ((fad:file-exists-p path) path)
      ((not (fad:file-exists-p path)) hunchentoot:+http-not-found+)
      (t path))))

(restas:define-route empty-route-for-ph-upload ("photo-upl-empty")
	"<h1> This is a stub page just for convenience of ajax photo uploader </h1>")

(restas:define-route person-change-route ("person:(id)" :parse-vars (list :id #'parse-integer))
	(person-change (get-person id *storage*)))

(defun get-persons (list)
  (iter (for p in list)
		(collect (get-person p *storage*))))


(restas:define-route wedding-change-route
					 ("wedding:(id)" :parse-vars (list :id #'parse-integer))
  (let ((wedd (get-wedding id *storage*)))
	(wedding-change wedd (if (m-man wedd) (get-person (m-man wedd) *storage*))
						 (if (m-wife wedd) (get-person (m-wife wedd) *storage*))
						 (get-persons (m-children wedd)))))

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
  (let ((person (init-from-alist 'person (hunchentoot:post-parameters*))))
    (alter-person person *storage*)
    (refresh-tree (item-owner person))))

(restas:define-route alter-marr ("alter-marriage" :method :post)
  (let ((marriage (init-from-alist 'marriage (hunchentoot:post-parameters*))))
    (alter-marriage marriage *storage*)
    (refresh-tree (item-owner marriage))))

(restas:define-route get-card ("card:(id)" :method :get :content-type "image/svg+xml")
  (person-card (get-person (parse-integer id) *storage*)))

(restas:define-route new-person ("new-person:(owner)" :method :get)
  (person-change (make-instance 'person :owner owner)))

(restas:define-route new-wedding ("new-wedding:(owner)" :method :get)
  (wedding-change (make-instance 'marriage :owner owner) nil nil nil))

(restas:define-route new-items ("new-items:(owner)")
  (draw-new-items owner))


