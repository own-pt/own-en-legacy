(in-package #:cl-wn-org)

(defclass synset ()
  ((lex-filenum :initarg :lex-filenum 
		:initform nil
		:accessor synset-lnum)
   
   (slot-pointers :initarg :slot-pointers
	        :initform nil
	        :accessor synset-slot-pointers)
   
   (pointers    :initarg :pointers 
	        :initform nil
	        :accessor synset-pointers)
   
   (words       :initarg :words 
		:initform nil
		:accessor synset-words)
   
   (gloss       :initarg :gloss
		:initform nil
		:accessor synset-gloss)))

