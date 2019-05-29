
(defpackage #:wn-data
  (:use #:cl #:alexandria #:serapeum)
  (:export #:synset-definition
	   #:synset-examples))

(defpackage #:wn-dsl
  (:use #:cl #:alexandria #:esrap #:wn-data #:serapeum)
  (:shadowing-import-from :serapeum :example)
  (:export #:read-wn
           #:get-sem-pointer
           #:synset-senses
           #:synset-file))

