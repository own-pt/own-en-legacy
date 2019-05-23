
(asdf:defsystem #:wn
  :description "Reading and Writing functions for a text-based Wordnet"
  :author "Alexandre Rademaker <alexrad@br.ibm.com> and Fabricio Chalub <fchalub@br.ibm.com>"
  :license "Apache 2.0"
  :serial t
  :depends-on (#:cl-ppcre #:alexandria #:wilbur #:split-sequence #:esrap)
  :components ((:file "package")
               (:file "data")
	       (:file "dsl-grammar")
	       ;; (:file "dsl-read")
	       ;; (:file "link")
	       ;; (:file "export")
	       ))
