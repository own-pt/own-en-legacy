
(in-package #:wn-data)


(defstruct sense (:print-function
		  (lambda (s st *)
		    (format st "<sense ~a::~a>"
			    (sense-word s) (sense-id s))))
  word
  id
  pointers)


(defstruct synset (:print-function
		   (lambda (s st *)
		     (format st "<synset ~a@~a ~{~a~^,~} ~a>"
			     (synset-source s) (synset-position synset)
			     (mapcar #'sense-word (synset-senses s))
			     (or (first (synset-gloss s)) ""))))
  source
  position
  senses
  pointers
  gloss)


(defun synset-definition (s)
  (first (synset-gloss s)))


(defun synset-examples (s)
  (rest (synset-gloss s)))
