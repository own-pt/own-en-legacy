
(in-package #:wordnet-dsl)

(defclass synset ()
  ((file :initarg :file 
	 :initform nil
	 :accessor synset-file)
   
   (lineno :initarg :line
	   :initform nil
	   :accessor synset-line)

   (lines :initarg :lines
	  :initform nil
	  :accessor synset-lines)

   (senses  :initarg :senses
	    :initform nil
	    :accessor synset-senses)

   (pointers    :initarg :pointers 
	        :initform nil
	        :accessor synset-pointers)))

(defun synset-gloss (s)
  (cdr (assoc "g" (synset-lines s) :test #'equal)))

(defun get-sem-pointer (s pointer)
  (let ((pointers (synset-pointers s)))
    (remove-if-not (lambda (x)
                     (equal (second x) pointer))
                   pointers)))
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

