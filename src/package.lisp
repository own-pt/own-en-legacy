
(defpackage #:wordnet-dsl
  (:use #:cl :cl-ppcre)
  (:export #:read-wn
	   #:make-local-links
	   #:make-external-links
	   #:txt->rdf))

