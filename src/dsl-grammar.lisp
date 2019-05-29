(in-package #:wn-dsl)

;; TODO: performance is not very good; if we will use it for
;; validation, it would be nice to have it be faster, under 1s

(setf esrap:*on-left-recursion* :error)

(defun chars->string (cs)
  (coerce cs 'string))

(defun wrap-sense (ws)
  (destructuring-bind (w . lex-id) ws
    (if (cl-ppcre:scan "^(noun|verb|adj|adjs|adv)\." w)
	(append (split-sequence-if (op (member _ '(#\. #\:) :test #'eql)) w :count 3)
		lex-id)
	ws)))

(defun valid-pointer-name? (str)
  (string/= str "frame"))

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

;; TODO: should these be ordered?
(defrule statement (and (or word-stmt gloss-stmt pointer-stmt frame-stmt)
			(? spaces))
  (:function first))

(defrule word-stmt (and "w:" spaces word-sense
			(? (* word-pointer))
			(? word-frames))
  (:destructure (* * ws ptrs frames)
		(list 'word ws ptrs frames)))

(defrule word-sense (and word (? lex-id))
  (:destructure (w lid?)
		(cons w (or lid? 0))))

(defrule word-pointer (and (valid-pointer-name? pointer-name) spaces word-sense)
  (:destructure (ptr * target)
		(list 'wpointer ptr (wrap-sense target))))

(defrule word-frames (and "frame" spaces (+ integer))
  (:destructure (* * fs)
		(cons 'wframes fs)))

(defrule word (and (+ (not whitespace)) (? spaces))
  (:destructure (cs *)
		(chars->string cs)))

(defrule lex-id integer)

(defrule integer (and (+ (character-ranges (#\0 #\9))) (? spaces))
  (:destructure (cs *)
    (parse-integer (chars->string cs) :radix 10)))

(defrule pointer-stmt (and pointer-name #\: spaces word-sense)
  (:destructure (ptr * * ws)
		(list 'spointer ptr (wrap-sense ws))))

(defrule pointer-name (+ (not (or #\: whitespace)))
  (:function chars->string))

(defrule gloss-stmt (or definition-stmt example-stmt))

(defrule text (+ (not linebreak))
  (:text t))

(defrule definition-stmt (and "g:" spaces text)
  (:destructure (* * def)
		(cons 'definition def)))

(defrule example-stmt (and "e:" spaces text)
  (:destructure (* * e)
	(cons 'example e)))

(defrule frame-stmt (and "frame:" spaces (+ (and integer (? spaces))))
  (:destructure (* * is)
		(cons 'sframes (mapcar #'first is))))

(defrule comment (and #\# (? spaces) text)
   (:function third)
   (:function chars->string)
  (:lambda (c) (cons 'comment c)))

;; utils
(defun parse-source (source-name)
  (multiple-value-bind (* vec)
      (cl-ppcre:scan-to-strings "^(noun|verb|adj|adjs|adv)\.([^:\.]+)" source-name)
    (coerce vec 'list)))

;; interface
(defun parse-lex (source-name text)
  (list* 'source source-name
	 (parse 'source text)))

(defun parse-lexfile (fp)
  (when (probe-file fp)
    (parse-lex (parse-source (pathname-name fp))
	       (uiop:read-file-string fp))))

