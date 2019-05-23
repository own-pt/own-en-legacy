(in-package #:wordnet-dsl)

;; TODO: performance is not very good; if we will use it for
;; validation, it would be nice to have it be faster, under 1s

(setf esrap:*on-left-recursion* :error)

(defun chars->string (cs)
  (coerce cs 'string))

(defun pointer? (cs)
  (member (chars->string cs)
	  *pointers-ids* :test #'equal))

;;; Utility rules.

(esrap:defrule spaces (+ (or #\Space #\Tab))
  (:constant nil)
  (:error-report :detail))

(defrule linebreak (and (? #\Return) #\Newline)
  (:constant nil)
  (:error-report :detail))

(defrule whitespace (+ (or #\Space #\Newline #\Tab #\Return)))


;; synset

(defrule source (and (? whitespace)
		     (* (and synset (+ (and linebreak (? spaces)))))
		     (? whitespace))
  (:function second)
  (:lambda (ss) (mapcar #'first ss)))

(defrule synset (+ stmt-or-comment)
  (:lambda (stms &bounds beg end)
    (list* 'synset (list beg end) stms)))

(defrule stmt-or-comment (and (or statement comment) linebreak (? spaces))
  (:function first))

(defrule statement (and (or word-stmt gloss-stmt pointer-stmt)
			(? spaces))
  (:function first))

(defrule word-stmt (and "w:" spaces word-sense (? (and spaces (* word-pointer))))
  (:destructure (* * w ptrs)
    (list* 'word w (second ptrs))))

(defrule word (+ (not whitespace))
  (:function chars->string))

(defrule word-sense (and word (? (and spaces lex-id)))
  (:destructure (w lid?)
		(if lid?
		    (cons w (second lid?))
		    w)))

(defrule lex-id (+ (character-ranges (#\0 #\9)))
  (:lambda (cs)
    (parse-integer (chars->string cs) :radix 10)))

(defrule word-pointer (and pointer-key spaces word-sense (? spaces))
  (:destructure (ptr * target *)
		(cons ptr target)))

(defrule pointer-stmt (and pointer-key #\: spaces word-sense)
  (:destructure (k * * w)
		(list* 'pointer k w)))

(defrule pointer-key (and (pointer? (+ (not (or #\: spaces)))))
  (:function first)
  (:function chars->string))

(defrule gloss-stmt (or definition-stmt example-stmt))

(defrule definition-stmt (and "g:" spaces text)
  (:destructure (* * def)
    (cons 'definition def)))

(defrule example-stmt (and "e:" spaces text)
  (:destructure (* * e)
	(cons 'example e)))

(defrule text (+ (not linebreak))
  (:text t))

(defrule comment (and #\# (? spaces) text)
  (:function third)
  (:function chars->string)
  (:lambda (c) (cons 'comment c)))

;; interface
(defun parse-lex (source-name text)
  (list* 'source source-name
	 (parse 'source text)))

(defun parse-lexfile (fp)
  (when (probe-file fp)
    (parse-lex (pathname-name fp)
	       (uiop:read-file-string fp))))
