
(require 'asdf)

(asdf:operate 'asdf:load-op '#:kin)

(restas:start '#:kin-package :port 8080)
