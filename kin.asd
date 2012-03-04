
(defsystem kin
	:depends-on (#:restas #:closure-template
				 #:percl)
	:components ((:module "src"
				  :components 
				  ((:file "tree")
				   (:file "arrange-tree" :depends-on ("tree"))
				   (:file "defmodule" :depends-on ("tree"))
				   (:file "database" :depends-on ("defmodule"))
				   (:file "convert-tree" :depends-on ("database" "tree" "arrange-tree" "defmodule"))
				   (:file "presenter" :depends-on ("defmodule" "convert-tree"))
				   (:file "routes" :depends-on ("presenter"))))))
