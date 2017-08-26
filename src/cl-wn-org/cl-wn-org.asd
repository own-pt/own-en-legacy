
(asdf:defsystem #:cl-wn-org
  :description "Describe lib here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:cl-ppcre)
  :components ((:file "package")
               (:file "data")
	       (:file "read")
	       (:file "link")
	       (:file "txt-to-rdf")))

