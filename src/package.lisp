
(defpackage #:wordnet-dsl
  (:use #:cl :cl-ppcre #:alexandria)
  (:export #:read-wn
           #:get-sem-pointer
           #:synset-senses
	   #:make-local-links
	   #:make-external-links
	   #:txt->rdf))

