
(defsystem "ip-management"
  :version "0.0.1"
  :maintainer "Fermin MF <fmfs@posteo.net>"
  :depends-on (#:iolib/streams #:trivial-gray-streams)
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "interface")
               (:file "process")
               (:file "utils")))
