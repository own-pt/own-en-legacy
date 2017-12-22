
(defpackage #:wordnet-dsl
  (:use #:cl :cl-ppcre #:alexandria #:wilbur #:split-sequence)
  (:export #:read-wn
           #:get-sem-pointer
           #:synset-senses
           #:synset-file))

