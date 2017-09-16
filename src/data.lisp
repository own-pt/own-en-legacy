(in-package #:cl-wn-org)

(defclass synset ()
  ((lex-file :initarg :lex-file 
		:initform nil
		:accessor synset-lex-file)
   
   (slot-pointers :initarg :slot-pointers
	        :initform nil
	        :accessor synset-slot-pointers)
   
   (pointers    :initarg :pointers 
	        :initform nil
	        :accessor synset-pointers)
   
   (senses      :initarg :words 
		:initform nil
		:accessor synset-senses)
   
   (gloss       :initarg :gloss
		:initform nil
		:accessor synset-gloss)
   (w           :initarg :w
		:initform nil
		:accessor synset-w)))


(defclass sense ()
  ((word             :initarg :word 
		     :initform nil
		     :accessor sense-word)
   (id               :initarg :id 
		     :initform nil
		     :accessor sense-id)
   (links-targets    :initarg :links-targets 
		     :initform nil
		     :accessor sense-links-targets)))

