(defsystem kin
	:depends-on (#:restas #:closure-template
				 #:mongo-cl-driver)
	:components ((:module "src"
				  :components ((:file "defmodule")
							   (:file "database" :depends-on ("defmodule"))
							   (:file "tree" :depends-on ( "defmodule"))
							   (:file "arrange-tree" :depends-on ("tree" "defmodule"))
							   (:file "convert-tree" :depends-on ("database" "tree" "arrange-tree" "defmodule"))
							   (:file "presenter" :depends-on ("defmodule" "convert-tree"))
							   (:file "routes" :depends-on ("presenter"))))))
