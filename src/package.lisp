
(defpackage #:wordnet-dsl
  (:use #:cl :cl-ppcre #:alexandria)
  (:export #:read-wn
	   #:make-local-links
	   #:make-external-links
	   #:txt->rdf))

