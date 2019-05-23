
(in-package #:wn-data)


(defstruct (sense (:print-function
		   (lambda (s st *)
		     (format st "<sense ~a::~a>"
			     (sense-word s) (sense-id s)))))
	   word
	   id
	   pointers)


(defstruct (synset (:print-function
		    (lambda (s st *)
		      (trivia:match s
			((synset source position senses pointers gloss)
			 (format st "<synset ~a@~a ~{~a~^,~} ~a>"
				 source position (mapcar #'sense-word senses)
				 (or (first gloss) "")))))))
  source
  position
  senses
  pointers
  gloss)


(defun synset-definition (s)
  (first (synset-gloss s)))


(defun synset-examples (s)
  (rest (synset-gloss s)))
