
(in-package #:wn-data)


(defstruct (synset (:print-function
		    (lambda (s st *)
		      (trivia:match s
			((synset source position senses gloss)
			 (format st "<synset ~a@~a {~{~a~^,~}} ~a>"
				 source position (mapcar #'car senses)
				 (or (first gloss) "")))))))
  source
  position
  senses				; (word . lex-id)
  gloss)


(defun synset-definition (s)
  (first (synset-gloss s)))


(defun synset-examples (s)
  (rest (synset-gloss s)))


(defparameter *wn-sem-relations* 		; (map synset relation)
  (make-hash-table :test 'equal :size 300000))

(defparameter *wn-lex-relations* 		; (map sense relation)
  (make-hash-table :test 'equal :size 300000))

(defgeneric @get-relation (synset))

;; (defmethod @get-relation ((obj synset))
;;   (gethash (sense-)))
