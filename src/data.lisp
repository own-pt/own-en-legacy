
(in-package #:wn-data)


(defstruct (synset (:print-function
		    (lambda (s st *)
		      (trivia:match s
			((synset source position senses gloss)
			 (format st "<synset ~a@~a {~{~a~^,~}} ~a>"
				 source position (mapcar #'car senses)
				 (or (ellipsize (first gloss) 10) "")))))))
  source
  position
  senses				; (word . lex-id)
  rels
  gloss)


(defun synset-definition (s)
  (first (synset-gloss s)))


(defun synset-examples (s)
  (rest (synset-gloss s)))


(defun synset-lexfilenum (s)
  (destructuring-bind (pos . lname)
      pos lname
    ;; TODO: this function
    42))


(defun synset-lexfilenum (s)
  (destructuring-bind (pos . *)
      ;; TODO: output should be [nvrsa]
      pos))


(defparameter *wn-sem-relations*	; (map synset relation)
  (make-hash-table :test 'equal :size 300000))


(defparameter *wn-lex-relations*	; (map sense relation)
  (make-hash-table :test 'equal :size 300000))


(defun is-reflexive? (rel)
  ;; TODO:
  t)


(defgeneric @get-relation (synset))

;; (defmethod @get-relation ((obj synset))
;;   (gethash (sense-)))
