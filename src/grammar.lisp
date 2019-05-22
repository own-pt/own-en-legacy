(cl:require :esrap)

(cl:defpackage #:lexfile-grammar
  (:use #:cl #:esrap))

(setf esrap:*on-left-recursion* :error)

(cl:in-package #:lexfile-grammar)

;;; A semantic predicate for filtering out double quotes.

(defun not-doublequote (char)
  (not (eql #\" char)))

(defun not-integer (string)
  (unless (find-if #'digit-char-p string)
    t))

;;; Utility rules.

(defrule spaces (+ (or #\Space #\Tab))
  (:constant nil)
  (:error-report :detail))

(defrule linebreak (and (? #\Return) #\Newline)
  (:constant nil)
  (:error-report :detail))

(defrule whitespace (+ (or #\Space #\Newline #\Tab #\Return)))


;; synset

(defrule source (and (? whitespace) (* synset))
  (:function second))

(defrule synset (and (+ stmt-or-comment) linebreak (? spaces))
  (:function first))

(defrule stmt-or-comment (and (or statement comment) linebreak (? spaces))
  (:function first))

(defrule statement (and (or word-stmt pointer-stmt gloss-stmt)
			(? spaces))
  (:function first))

(defrule word-stmt (and "w:" spaces word (? spaces) (* word-pointer))
  (:destructure (* * w * ptrs)
    (if ptrs
	(cons w ptrs)
	w)))

(defrule word (+ (not whitespace))
  (:lambda (w) (coerce w 'string)))

(defrule word-pointer (and word spaces word (? spaces))
  (:destructure (ptr * target)
		(cons ptr target)))

(defrule pointer-stmt (and pointer-key #\: spaces word)
  (:destructure (k * * w)
		(cons k w)))

(defrule pointer-key (or "sim" "hyper"))

(defrule gloss-stmt (and definition (* example))
  (:destructure (def exs)
		(cons def exs)))

(defrule definition (and "g:" spaces text)
  (:function third))

(defrule example (and "e:" spaces text)
  (:function third))

(defrule text (+ (not linebreak))
  (:text t))

(defrule comment (and #\# (? spaces) text)
  (:function third)
  (:lambda (s) (coerce s 'string)))

