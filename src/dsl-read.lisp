(in-package #:wn-dsl)

;; pipeline: parse -> process -> link
;;;
;; parse: lexfile -> source-tree ----------------------------------
;; return source tree dsl-grammar
;;; 
;; process: source-tree -> intermediary trie ----------------------
;; each active node has as car a synset (if the key is a head word) or
;; the name of the head word of a synset, and as cdr the unprocessed
;; relations
;;;
;; link: intermediary trie -> final trie


(defun new-trie ()
  (make-instance 'cl-trie:trie :key nil))


(defun trie-fold (fn init trie)
  "FN takes as input a key, a value, and the accumulated result."
  (labels
      ((run (key acc trie)
	 (let* ((c       (cl-trie:key trie))
		(new-key (concatenate 'string key (string (or c "")))))
	   (reduce (lambda (acc trie) (run new-key acc trie))
		   (cl-trie:children trie)
		   :initial-value (if (cl-trie:activep trie)
				      (funcall fn
					       new-key
					       (cl-trie:value trie)
					       acc)
				      acc)))))
    (run "" init trie)))


(defun get-synset (db word-id)
  (labels
      ((f (word-id final)
	 (let ((found (cl-trie:lookup db word-id)))
	   (when found
	     (if (wn-data::synset-p found)
		 found
		 (if final
		     (error "Bad reference")
		     (f found t)))))))
    (f word-id nil)))


(defun synset-add-rel (s rel target-id)
  (let ((rels (wn-data::synset-rels s)))
    (setf (wn-data::synset-rels s)
	  (cons (cons rel target-id) rels))
    s))


(defun synset-add-lexrel (s head-id rel target-id)
  (let* ((senses    (wn-data::synset-senses s))
	 (head-word (take-while (op (char/= #\Tab _)) head-id))
	 (i         (or (position head-word senses :key #'car :test #'string-equal)
			(error "bad reference")))
	 (old       (elt senses i)))
    (destructuring-bind (w ptrs frames) old
	(let ((new (list w (cons (cons rel target-id) ptrs) frames)))
	  (setf (wn-data::synset-senses s) (replace senses (list new) :start1 i))))
    s))


(defun add-rel (db head rel target &optional is-sem? add-refl?)
  (when add-refl?
    (add-rel db target rel head))
  (let* ((head-sy (get-synset db head))
	 (head-sy (if is-sem?
		      (synset-add-rel head-sy rel target)
		      (synset-add-lexrel head-sy head rel target))))
    (cl-trie:insert head-sy db head)))


;; TODO: could we use wilbur for all of this?

;; TODO: source should be integer for better memory footprint;
;; relations too, could be symbols or integers (printing should
;; convert them, of course) -- or we could use strings & tries for
;; everything (might be easier to implement)
(defun process-source (source-tree &optional (trie (new-trie)))
  (destructuring-bind (* (source-pos source-lname) . synsets) source-tree
    (labels
	((to-id (pos lname word lex-id)
	   (format nil "~a	~a	~a	~a" word pos lname lex-id))
	 (word-id (ws)
	   (trivia:match ws
	     ((list* pos lname w lid) (to-id pos lname w lid))
	     ((list* w lid)           (to-id source-pos source-lname w lid))))
	 (p-relation (head ptr)
	   (destructuring-bind (label rel target) ptr
	     (let ((target-id (word-id target)))
	       (destructuring-bind (s . ptrs) (cl-trie:lookup trie head)
		 (cl-trie:insert (cons s
				       (cons (list label rel target-id)
					     ptrs))
				 trie head)))))
	 (p-sense (head w)
	   (destructuring-bind (* ws ptrs wframes) w
	     (let ((wid (word-id ws)))
	       (unless (equal wid head)
		 (cl-trie:insert (list head) trie wid))
	       ;; TODO: handle wframes
	       (mapc (curry #'p-relation wid) ptrs))
	     ws))
	 (p-synset (sy)
	   (destructuring-bind (* position . stmts) sy
	     (let ((groups (serapeum:partitions (list (op (eq  _ 'word))
						      ;; TODO: handle sframes
						      (op (eq  _ 'spointer))
						      (op (memq _ '(definition example))))
						stmts :key #'first)))
	       (destructuring-bind (((label ws . w-ptrs) . senses) pointers gloss) groups
		 (let ((id     (word-id ws))
		       (senses (cons (list* label ws w-ptrs) senses)))
		   (cl-trie:insert
		    (list (wn-data::make-synset
			   :source   (cons source-pos source-lname)
			   :position position
			   :senses   (mapcar (op (list (car (second _)) nil nil)) senses)
			   :gloss    (mapcar #'cdr gloss))) ; TODO: not making sure definition comes first
		    trie id)
		   (mapc (curry #'p-sense id) senses)
		   (mapc (curry #'p-relation id) pointers)))))))
      ;; 
      (mapcar #'p-synset synsets)
      trie)))


(defun link (itrie)
  "Take (intermediary) trie whose values are (cons (or synsetp stringp) pointers),
add the pointers to the appropriate places, and return new trie whose
values are (or synsetp stringp)."
  (labels
      ((add-ptrs (sy head ptr)
	 (trivia:match ptr
	   ((list* 'spointer rel target)
	    (synset-add-rel sy rel target))
	   ((list* 'wpointer rel target)
	    (synset-add-lexrel sy head rel target))))
       (link (k v acc)
	 (destructuring-bind (s . ptrs) v
	   (let ((s (if (wn-data::synset-p s) s (first (cl-trie:lookup itrie s)))))
	     (mapc (curry #'add-ptrs s k) ptrs))
	   (cl-trie:insert s acc k)
	   acc)))
    (trie-fold #'link (new-trie) itrie)))


;;; Conversions, see wndb(5WN) for desired output format
(defun db->wndb (db)
  (labels
      ((flush (l p ss)
	 (let ((synset_cnt (length ss)))
	   (format nil
		   "~a ~a ~a ~a~{ ~a~} ~a ~a~{ ~a~}"
		   (string-downcase l)
		   p
		   synset_cnt
		   ;; TODO: how to calculate this?
		   0
		   nil
		   synset_cnt
		   ;; TODO: deprecate?
		   0
		   ;; TODO: how to calc?
		   nil)))
       (f (k v acc)
	 (destructuring-bind (lemma pos * *) (split-sequence #\Tab k)
	   (if acc
	       (destructuring-bind ((l . p) . ss) acc
		 (if (and (string= l lemma)
			  (string= p pos))
		     (list (cons l p) (cons v ss))
		     (progn
		       (flush l p ss)
		       (list (cons lemma pos) v))))
	       (list (cons lemma pos) v)))))
    (trie-fold #'f nil db)))


(defun db->wndb-data (db)
  ;; order of wndb seems to be that of lexfiles and the order in which
  ;; the synsets appear in them; we need not follow this, since it
  ;; doesn't seem to have any reason
  (labels
      ((f (s i)
	 ;;  synset_offset  lex_filenum  ss_type  w_cnt  word  lex_id  [word  lex_id...]  p_cnt  [ptr...]  [frames...]  |   gloss
	 (format nil
		 "~a ~a ~a ~X ~:{~a ~a ~}~3,'0d ~{~a~} ~:{~a ~a ~a ~2,'0d~2,'0d ~} ~:[~;~2,'0d~]~:{ + ~2,'0d ~2,'0x~} |~{ ~a~}"
		 i				; synset offset
		 (wn-data::synset-lexfilenum s) ; lex_filenum
		 (wn-data::synset-pos s)	; ss_type
		 (length (synset-senses s))	; w_cnt
		 ;; TODO: lex_id should be hexadecimal, is it?
		 (mapcar (op (list (car _) (cdr _))) (synset-senses s)) ; word lex_id ...
		 (length (wn-data::synset-rels s)) ; p_cnt
		 ;; TODO: pointers
		 '((#\~ 3984384 n 0 0))	; ptr :: ptr_symbol synset_offset pos source/target
		 nil 			; frames :: f_cnt
		 '() 			; frames :: + f_num w_num ...
		 ;; gloss
		 (wn-data::synset-gloss s))))))

