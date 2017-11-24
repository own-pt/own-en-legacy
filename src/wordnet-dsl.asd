
(asdf:defsystem #:wordnet-dsl
  :description "Reading and Writing functions for a text-based Wordnet"
  :author "Alexandre Rademaker <alexrad@br.ibm.com> and Fabricio Chalub <fchalub@br.ibm.com>"
  :license "CC-BY"
  :serial t
  :depends-on (#:cl-ppcre #:alexandria)
  :components ((:file "package")
               (:file "data")
	       (:file "read")
	       (:file "link")
	       (:file "txt-to-rdf")))

