
(defpackage #:wn-dsl
  (:use #:cl :cl-ppcre #:alexandria #:esrap)
  (:export #:read-wn
           #:get-sem-pointer
           #:synset-senses
           #:synset-file))


(defpackage #:wn-data
  (:use #:cl :cl-ppcre #:alexandria #:esrap)
  (:export #:read-wn
           #:get-sem-pointer
           #:synset-senses
           #:synset-file))
