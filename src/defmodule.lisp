(restas:define-module #:kin-package
   (:use #:cl #:iter )
   (:import-from #:mongo-cl-driver.son-sugar son)
   (:export ))

(in-package #:kin-package)

(defparameter *templates-path* (merge-pathnames "src/presenter.tmpl"
											   (asdf:component-pathname (asdf:find-system '#:kin))))

(setq hunchentoot:*show-lisp-errors-p* t)
(setq hunchentoot:*tmp-directory* #P"/home/necto/tmp/")

(defun compile-tmpl ()
	(closure-template:compile-template
	  :common-lisp-backend *templates-path*))

(compile-tmpl)
