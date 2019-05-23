
(asdf:defsystem #:wordnet-dsl
  :description "Reading and Writing functions for a text-based Wordnet"
  :author "Alexandre Rademaker <alexrad@br.ibm.com> and Fabricio Chalub <fchalub@br.ibm.com>"
  :license "Apache 2"
  :serial t
  :depends-on (#:cl-ppcre #:alexandria #:wilbur #:split-sequence #:esrap)
  :components ((:file "package")
               (:file "data")
	       (:file "grammar")
	       (:file "read")
	       (:file "link")
	       (:file "export")))

