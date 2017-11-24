(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)
(ql:quickload :fare-csv)

(defpackage :wntotext
  (:use :cl :alexandria))

(in-package :wntotext)

(defparameter *pointers* '(("n" . (("!" "ant" "Antonym")
                                   ("@" "hyper" "Hypernym")
                                   ("@i" "ihyper" "Instance Hypernym")
                                   ("~" "hypo" "Hyponym")
                                   ("~i" "ihypo" "Instance Hyponym")
                                   ("#m" "hm" "Member holonym")
                                   ("#s" "hs" "Substance holonym")
                                   ("#p" "hp" "Part holonym")
                                   ("%m" "mm" "Member meronym")
                                   ("%s" "ms" "Substance meronym")
                                   ("%p" "mp" "Part meronym")
                                   ("=" "attr" "Attribute")
                                   ("+" "drf" "Derivationally related form")
                                   (";c" "dt" "Domain of synset - TOPIC")
                                   ("-c" "mt" "Member of this domain - TOPIC")
                                   (";r" "dr" "Domain of synset - REGION")
                                   ("-r" "mr" "Member of this domain - REGION")
                                   (";u" "du" "Domain of synset - USAGE")
                                   ("-u" "mu" "Member of this domain - USAGE")))
                           ("v" . (("!" "ant" "Antonym")
                                   ("@" "hyper" "Hypernym")
                                   ("~" "hypo" "Hyponym")
                                   ("*" "entail" "entailment")
                                   (">" "cause" "Cause")
                                   ("^" "see" "Also see")
                                   ("$" "vg" "Verb Group")
                                   ("+" "drf" "Derivationally related form")
                                   (";c" "dt" "Domain of synset - TOPIC")
                                   (";r" "dr" "Domain of synset - REGION")
                                   (";u" "du" "Domain of synset - USAGE")))
                           ("a" . (("!" "ant" "Antonym")
                                   ("&" "sim" "Similar to")
                                   ("<" "pv" "Participle of verb")
                                   ("\\" "pe" "Pertainym (pertains to noun)")
                                   ("=" "attr" "Attribute")
                                   ("^" "see" "Also see")
                                   ("+" "drf" "Derivationally related form")
                                   (";c" "dt" "Domain of synset - TOPIC")
                                   (";r" "dr" "Domain of synset - REGION")
                                   (";u" "du" "Domain of synset - USAGE")))
                           ("s" . (("!" "ant" "Antonym")
                                   ("&" "sim" "Similar to")
                                   ("<" "pv" "Participle of verb")
                                   ("\\" "pe" "Pertainym (pertains to noun)")
                                   ("=" "attr" "Attribute")
                                   ("^" "see" "Also see")
                                   ("+" "drf" "Derivationally related form")
                                   (";c" "dt" "Domain of synset - TOPIC")
                                   (";r" "dr" "Domain of synset - REGION")
                                   (";u" "du" "Domain of synset - USAGE")))
                           ("r" . (("!" "ant" "Antonym")
                                   ("\\" "da" "Derived from adjective")
                                   (";c" "dt" "Domain of synset - TOPIC")
                                   (";r" "dr" "Domain of synset - REGION")
                                   (";u" "du" "Domain of synset - USAGE")))))

(defparameter *pos* '(("n" "noun" 1 "Nouns") 
                      ("v" "verb" 2 "Verbs") 
                      ("a" "adj" 3 "Adjectives") 
                      ("s" "adjs" 5 "Satellite adjectives")
                      ("r" "adv" 4 "Adverbs")))

(defparameter *lex-filenum* '((0 "adj.all" "all adjective clusters (minus satellites)")
                              (100 "adjs.all" "all adjective clusters (satellites)")
                              (1 "adj.pert" "relational adjectives (pertainyms)")
                              (2 "adv.all" "all adverbs")
                              (3 "noun.Tops" "unique beginner for nouns")
                              (4 "noun.act" "nouns denoting acts or actions")
                              (5 "noun.animal" "nouns denoting animals")
                              (6 "noun.artifact" "nouns denoting man-made objects")
                              (7 "noun.attribute" "nouns denoting attributes of people and objects")
                              (8 "noun.body" "nouns denoting body parts")
                              (9 "noun.cognition" "nouns denoting cognitive processes and contents")
                              (10 "noun.communication" "nouns denoting communicative processes and contents")
                              (11 "noun.event" "nouns denoting natural events")
                              (12 "noun.feeling" "nouns denoting feelings and emotions")
                              (13 "noun.food" "nouns denoting foods and drinks")
                              (14 "noun.group" "nouns denoting groupings of people or objects")
                              (15 "noun.location" "nouns denoting spatial position")
                              (16 "noun.motive" "nouns denoting goals")
                              (17 "noun.object" "nouns denoting natural objects (not man-made)")
                              (18 "noun.person" "nouns denoting people")
                              (19 "noun.phenomenon" "nouns denoting natural phenomena")
                              (20 "noun.plant" "nouns denoting plants")
                              (21 "noun.possession" "nouns denoting possession and transfer of possession")
                              (22 "noun.process" "nouns denoting natural processes")
                              (23 "noun.quantity" "nouns denoting quantities and units of measure")
                              (24 "noun.relation" "nouns denoting relations between people or things or ideas")
                              (25 "noun.shape" "nouns denoting two and three dimensional shapes")
                              (26 "noun.state" "nouns denoting stable states of affairs")
                              (27 "noun.substance" "nouns denoting substances")
                              (28 "noun.time" "nouns denoting time and temporal relations")
                              (29 "verb.body" "verbs of grooming, dressing and bodily care")
                              (30 "verb.change" "verbs of size, temperature change, intensifying, etc.")
                              (31 "verb.cognition" "verbs of thinking, judging, analyzing, doubting")
                              (32 "verb.communication" "verbs of telling, asking, ordering, singing")
                              (33 "verb.competition" "verbs of fighting, athletic activities")
                              (34 "verb.consumption" "verbs of eating and drinking")
                              (35 "verb.contact" "verbs of touching, hitting, tying, digging")
                              (36 "verb.creation" "verbs of sewing, baking, painting, performing")
                              (37 "verb.emotion" "verbs of feeling")
                              (38 "verb.motion" "verbs of walking, flying, swimming")
                              (39 "verb.perception" "verbs of seeing, hearing, feeling")
                              (40 "verb.possession" "verbs of buying, selling, owning")
                              (41 "verb.social" "verbs of political and social activities and events")
                              (42 "verb.stative" "verbs of being, having, spatial relations")
                              (43 "verb.weather" "verbs of raining, snowing, thawing, thundering")
                              (44 "adj.ppl" "participial adjectives")))


(defparameter *redundant-pointers* '("~" "~i" "-c" "-r" "-u" "%m" "%s" "%p"))

(defparameter *senses* nil)
(defparameter *synsets* nil)
(defparameter *org-mode* nil)

;; Antonym	Antonym
;; Hyponym	Hypernym
;; Hypernym	Hyponym
;; Instance Hyponym	Instance Hypernym
;; Instance Hypernym	Instance Hyponym
;; Holonym	Meronym
;; Meronym	Holonym
;; Similar to	Similar to
;; Attribute	Attribute
;; Verb Group	Verb Group
;; Derivationally Related	Derivationally Related
;; Domain of synset	Member of Doman


(defclass synset ()
  ((id          :initarg :id 
		:initform nil
		:accessor synset-id)
   (lex-filenum :initarg :lex-filenum 
		:initform nil
		:accessor synset-lnum)
   (ss-type     :initarg :ss-type 
		:accessor synset-type)  
   (words       :initarg :words 
		:initform nil
		:accessor synset-words) 
   (pointers    :initarg :pointers 
		:initform nil
		:accessor synset-pointers)
   (gloss       :initarg :gloss
		:accessor synset-gloss)
   (frames      :initarg :frames 
		:initform nil
		:accessor synset-frames)
   (base        :initarg :base
		:initform nil
		:accessor synset-base)
   (notes       :initarg :notes 
		:initform nil
		:accessor synset-notes)))

(defun collect (data start size total)
  "Auxiliar function for collect subsequences of a sequence."
  (do ((pos start (+ pos size))
       (res nil)
       (count 1 (+ 1 count)))
      ((> count total)
       (reverse res))
    (push (subseq data pos (+ pos size)) res)))

(defun parse-word (word &optional (adj nil))
  (if (not (null adj))
      (multiple-value-bind (m g)
	  (cl-ppcre:scan-to-strings "(.*)\\((a|p|ip)\\)" (nth 0 word))
	(if (null m)
	    (list (nth 0 word) 
		  (parse-integer (nth 1 word) :radix 16) nil)
	    (list (aref g 0)
		  (parse-integer (nth 1 word) :radix 16) (aref g 1))))
      (list (nth 0 word) 
	    (parse-integer (nth 1 word) :radix 16) nil)))

(defun parse-pointer (ptr)
  (let ((hexval (nth 3 ptr))) 
    (append (subseq ptr 0 3) 
	    (list (parse-integer (subseq hexval 0 2) :radix 16)
		  (parse-integer (subseq hexval 2 4) :radix 16)))))

(defun parse-frame (frm)
  (list (parse-integer (nth 1 frm))
	(parse-integer (nth 2 frm) :radix 16)))

(defun parse-data-line (line)
  "It reads a line from data.{noun,verb,adv,adj} wordnet database file
   and returns a synset instance."
  (let* ((gloss-sep (position #\| line))
	 (data (cl-ppcre:split " " (subseq line 0 gloss-sep)))
	 (gloss (subseq line (+ 1 gloss-sep)))
	 (w-cnt (parse-integer (nth 3 data) :radix 16))
	 (p-cnt-pos (+ 4 (* 2 w-cnt)))
	 (p-cnt (parse-integer (nth p-cnt-pos data)))
	 (fields (+ 5 (* 2 w-cnt) (* 4 p-cnt)))
	 (f-cnt (if (> (length data) fields)
		    (parse-integer (nth fields data)) 0))
	 (words (if (search (nth 2 data) "as")  
		    (mapcar (lambda (w) (parse-word w t)) (collect data 4 2 w-cnt))
		    (mapcar #'parse-word (collect data 4 2 w-cnt)))))
    (make-instance 'synset 
		   :id (nth 0 data)
		   :lex-filenum (parse-integer (nth 1 data))
		   :ss-type (nth 2 data)
		   :gloss (string-trim '(#\Space) gloss)
		   :words words
		   :pointers (mapcar #'parse-pointer (collect data (+ p-cnt-pos 1) 4 p-cnt))
		   :frames (mapcar #'parse-frame (collect data (+ 1 fields) 3 f-cnt)))))

(defun parse-file (filename parser &optional (limit nil))
  "It reads a file {index,data}.{noun,verb,adj,adv} wordnet database."
  (with-open-file (f filename)
    (do* ((line (read-line f nil)
		(read-line f nil))
	  (parser? (string/= line "  " :end1 2)
		   (string/= line "  " :end1 2))
	  (counter 0 (if parser? 
			 (+ 1 counter) 
			 counter))
	  (res nil))
	 ((or (null line)
	      (and limit (> counter limit)))
	  (reverse res))
      (if parser? 
	  (let ((data (funcall parser line)))
	    (if data (push data res)))))))

;; parser for each kind of file

(defun parser-senseidx (line)
  (let* ((data (cl-ppcre:split " " line))
	 (key (nth 0 data))
	 (keyparts (cl-ppcre:split "%" key))
	 (lemma (car keyparts))
	 (keyrest (cl-ppcre:split ":" (cadr keyparts))))
    (list :key key 
	  :lemma lemma 
	  :ss-type    (parse-integer (nth 0 keyrest)) 
	  :lexfilenum (parse-integer (nth 1 keyrest)) 
	  :lexid      (parse-integer (nth 2 keyrest))
	  :synset       (nth 1 data) 
	  :sense-number (nth 2 data) 
	  :tag-count    (nth 3 data))))

(defun parser-sents (line)
  (multiple-value-bind (s a) 
      (cl-ppcre:scan-to-strings "([0-9]+)[ ]+(.*)" line)
    (declare (ignore s))
    (list (parse-integer (aref a 0)) 
	  (aref a 1))))

(defun parser-core (line)
  (multiple-value-bind (s a) 
      (cl-ppcre:scan-to-strings "([0-9]+)-([asrnv])" line)
    (declare (ignore s)) 
    (list :offset (aref a 0) :type (aref a 1))))

(defun parser-sentidx (line)
  (let ((data (cl-ppcre:split " " line)))
    (list :key (nth 0 data)
	  :examples (mapcar #'parse-integer (cl-ppcre:split "," (nth 1 data))))))

(defun parser-lexnames (line)
  (multiple-value-bind (m g) 
      (cl-ppcre:scan-to-strings "([0-9]+)[ \\t]+([a-zA-Z\\.]*)[ \\t]+([0-9]*)" line)
    (declare (ignore m)) 
    (list (parse-integer (aref g 0)) 
	  (aref g 1)
	  (parse-integer (aref g 2)))))







(defun read-morphosemantic-links (f)
  "Read morphosemantic links (exported as a CSV file) and add the
   appropriate pointers in the data structures."
  (flet ((find-word (lemma lexid s)
           (position-if  (lambda (x) (and (string-equal lemma (first x)) (= lexid (second x)))) (synset-words s))))
    (fare-csv:with-rfc4180-csv-syntax ()
      (dolist (row (cdr (fare-csv:read-csv-file f)))
        (destructuring-bind (arg1sensekey arg1offset relation arg2sensekey arg2offset arg1gloss arg2gloss) row
          (declare (ignore arg1gloss arg2gloss))
          (let* ((sense1 (parser-senseidx arg1sensekey))
                 (sense2 (parser-senseidx arg2sensekey))
                 (s1 (gethash (format nil "~a-v" (subseq arg1offset 1)) *synsets*))
                 (s2 (gethash (format nil "~a-n" (subseq arg2offset 1)) *synsets*))
                 (sense-info1 (gethash (getf sense1 :key) *senses*))
                 (sense-info2 (gethash (getf sense2 :key) *senses*)))
            (push (list relation (synset-id s2) "n" 
                        (1+  (seventh sense-info1)) 
                        (1+  (seventh sense-info2)))
                  (synset-pointers s1))))))))

(defun mk-sense1 (w)
  "Convert the tuple containing wordsense information to a wordsense
   specification.  The second element of the tuple is the lex_id.  If
   lex_id is 0 (the first sense in the lex. file), we simply return
   the word as the wordsense; otherwise we add the lex_id suffix to
   the word and use it as the wordsense.  See SENSEIDEX(5WN)/sense key
   encoding."
  (if *org-mode*
      (if (= 0 (second w))
          (format nil "<<~a>>" (first w))
          (format nil "<<~a~a>>" (first w) (second w)))
      (if (= 0 (second w))
          (format nil "~a" (first w))
          (format nil "~a~a" (first w) (second w)))))

(defun mk-sense (w)
  "Convert the tuple containing wordsense information to a wordsense
   specification.  The second element of the tuple is the lex_id.  If
   lex_id is 0 (the first sense in the lex. file), we simply return
   the word as the wordsense; otherwise we add the lex_id suffix to
   the word and use it as the wordsense.  See SENSEIDEX(5WN)/sense key
   encoding."
  (if (= 0 (second w))
      (format nil "~a" (first w))
      (format nil "~a~a" (first w) (second w))))

(defun lexfile-name (lnum)
  "Returns the name of the lexicographer file associated to number
   LNUM."
  (first (cdr (assoc lnum *lex-filenum* :test #'equal))))

(defun synset-tag (sc)
  "Returns the synset tag (used to start a new synset in the DSL) from the one-letter POS tag (nvrsa)"
  (cadr (assoc sc *pos* :test #'equal)))

(defun synset-type-to-suffix (ty)
  "Returns the one-letter POS tag from synset type number (12345)."
  (first (find ty *pos* :key #'third)))

(defun pointer-mnemonic (syncat p)
  "Returns the mnemonic conversion of pointer P from category SYNCAT."
  (let ((mn (cadr (assoc p (cdr (assoc syncat *pointers* :test #'equal)) :test #'equal))))
    (or mn p)))

(defun representative-word (s)
  "Returns the wordsense that will represent synset S."
  (format nil "~a" (mk-sense (first (synset-words s)))))

(defun link-to-synset (s1 s2)
  "Returns a link to synset S2.  If S1 and S2 are in the same
   lexicographer file, we only need the representative word; otherwise
   we need to specify the lex. file of S2."
  (let ((lnum1 (synset-lnum s1))
        (lnum2 (synset-lnum s2))
        (rw (representative-word s2)))
    (if *org-mode*
        (if (= lnum1 lnum2)
            (format nil "[[~a]]" rw)
            (format nil "[[file:~a.org::~a]]" (lexfile-name (synset-lnum s2)) rw))
        (if (= lnum1 lnum2)
            rw
            (format nil "~a:~a" (lexfile-name (synset-lnum s2)) rw)))))

(defun link-to-wordsense (s1 s2 n2)
  "Link from wordsense N1 of synset S1 to wordsense N2 of synset S2.
   If S1 and S2 are in the same lexicographer file, we only need the
   word; otherwise we need to specify the lex. file of S2."
  (let ((lnum1 (synset-lnum s1))
        (lnum2 (synset-lnum s2))
        (w2 (nth (1- n2) (synset-words s2))))
    (if *org-mode*
        (if (= lnum1 lnum2)
            (format nil "[[~a]]" (mk-sense w2))
            (format nil "[[file:~a.org::~a]]" (lexfile-name (synset-lnum s2)) (mk-sense w2)))
        (if (= lnum1 lnum2)
            (mk-sense w2)
            (format nil "~a:~a" (lexfile-name (synset-lnum s2)) (mk-sense w2))))))

(defun redundant? (p)
  "A pointer is REDUNDANT if its existence can be inferred from other
   pointers (like hypernym vs hyponym)"
  (member p *redundant-pointers* :test #'equal))

(defun semantic-relation? (p)
  "Indicates if pointer P is a semantic relation between synsets 
   (as opposed to a relation between words)"
  (= 0 (fourth p)) (= 0 (fifth p)))

(defun synset-sense-links (s)
  (remove-if #'semantic-relation? (synset-pointers s)))

(defun global-frame? (f)
  "TRUE is frame applies to all words in the synset."
  (= 0 (second f)))

(defun mk-sense-pointer (w pointers frames)
  "Given a wordsense W and a list of sense pointers POINTERS (which
   can be NIL), returns the combined form as follows: (a) if POINTERS
   is NIL, then returns just (w sense); (b) if POINTERS is not NIL,
   then returns (w sense <pointers>) where <pointers> is the pointer
   specification for this word (see MK-POINTER in the flet.)"
  (flet ((mk-frame (frames)
           (if frames
               (format nil "frame ~{~a~^ ~}" (mapcar #'first frames))
               ""))
         (mk-pointer (p)
           (format nil "~a ~a" (second p) (third p))))
    (format nil "w: ~a" (string-trim '(#\space) (format nil "~a ~{~a~^ ~} ~a" (mk-sense1 w)
							(mapcar #'mk-pointer pointers) (mk-frame frames))))))

(defun mk-senses (s)
  (flet ((resolve-frames-senses (fr)
           "Takes a frame specification and resolves it to the words in the synset."
           (let ((words (synset-words s)))
             (mapcar (lambda (x) (list (first x) (mk-sense (nth (1- (second x)) words)))) fr)))
         (find-sense-frames (w fr)
           "Find all frames that apply to that word"
           (remove-if-not (lambda (x) (equal (mk-sense w) (second x))) fr))
         (find-sense-pointers (w wp)
           "Return all pointers in WP related to sense W."
           (remove-if-not (lambda (x) (equal (mk-sense w) (first x))) wp))
         (process-sense-links (wl)
           "Returns a data structure containing the aggregate
            information about the sense links: pointer mnemonics are
            resolved; synset ids are converted to the synset objects;
            word refernces in frames are converted to actual words."
           (unless (redundant? (first wl))
             (let ((word (nth (1- (fourth wl)) (synset-words s)))
                   (pointer (pointer-mnemonic (synset-type s) (first wl)))
                   (dest (gethash (format nil "~a-~a" (second wl) (third wl)) *synsets*)))
               (list (mk-sense word) pointer (link-to-wordsense s dest (fifth wl)))))))
    (let ((sense-frames (resolve-frames-senses (remove-if #'global-frame? (synset-frames s))))
          (sense-links (mapcar #'process-sense-links (synset-sense-links s))))
      (format nil "~{~a~^~%~}~%"
              (mapcar (lambda (w) (mk-sense-pointer w
                                                    (find-sense-pointers w sense-links) 
                                                    (find-sense-frames w sense-frames)))
                      (synset-words s))))))

(defun mk-sem-pointer (p origin)
  "Create a semantic pointer between synsets."
  (when (and (not (redundant? (first p))) (semantic-relation? p))
    (let ((dest (gethash (format nil "~a-~a" (second p) (third p)) *synsets*)))
     (format nil "~a: ~a" (pointer-mnemonic (synset-type origin) (first p)) (link-to-synset origin dest)))))

(defun mk-sem-pointers (s)
  (let ((sem-pointers (remove nil (mapcar (lambda (x) (mk-sem-pointer x s)) (synset-pointers s)))))
    (if sem-pointers
	(format nil "~{~a~^~%~}~%" sem-pointers)
	"")))

(defun add-synset (s)
  (flet ((fix-adjective (s)
           "We need to change the type from S to A because the data
            files don't reference satellite adjectives via S (which is
            an inconsistency).  Also, we add 100 to the lex file
            number so we can save the satellites in their own
            namespace."
           (when (equal "s" (synset-type s))
             (setf (synset-type s) "a")
             (setf (synset-lnum s) (+ 100 (synset-lnum s))))))
    (fix-adjective s)
    (let* ((key (format nil "~a-~a" (synset-id s) (synset-type s))))
      (setf (gethash key *synsets*) s))))

(defun update-words-lex-ids (words wids)
  "Update the LEX_IDs of all words in WORDS, according to the hash
   table WIDS.  The hash table contains the current count for each
   lemma."
  (let ((new-words))
    (dolist (w words)
      (let ((max-id (gethash (first w) wids)))
        (if max-id
            (setf (gethash (first w) wids) (1+ max-id))
            (setf (gethash (first w) wids) 0))
        (setf (second w) (gethash  (first w) wids)))
      (push w new-words))
    (reverse new-words)))

(defun fix-lex-ids (lnum)
  "We can't guarantee that the combination of LEMMA+LEX_ID [wndb(5WN)]
   gives us an unique sense ID for all lex. files.  This rule is
   violated in the satellite adjectives, unfortunately.  The easiest
   solution is to remove all lex_ids and replace them with our own.
   Since pointers use a different indexing scheme (synset offset +
   word number), this is not a problem.  We need to make sure that we
   have the correct mapping to sense keys. This is done by the
   ADD-SENSEIDX function.  Note that this is done per lexfile (LNUM),
   since we only need to guarantee uniqueness per lex file and not
   globally."
  (let ((wids (make-hash-table :test #'equal)))
    (dolist (s (hash-table-values *synsets*))
      (when (eq lnum (synset-lnum s))
        (let ((words (synset-words s)))
          (setf (synset-words s) (update-words-lex-ids words wids)))))))

(defun mk-frames (s)
  "Generate the global frame information. A frame is GLOBAL if it
applies to all the senses in the synset."
  (let ((frames (remove-if-not #'global-frame? (synset-frames s))))
    (if frames 
        (format nil "frame: ~{~a~^ ~}~%" (mapcar #'first frames))
        "")))

(defun mk-synset (stream sid)
  (let ((s (gethash sid *synsets*)))
    (format stream
            "~a~a~ag: ~a~%~%" (mk-senses s) (mk-sem-pointers s) (mk-frames s) (synset-gloss s))))

(defun add-senseidx (s)
  (let* ((key (getf s :key))
         (suffix (synset-type-to-suffix (getf s :ss-type)))
         (synset-id (format nil "~a-~a" (getf s :synset) (if (equal suffix "s") "a" suffix)))
         (synset (gethash synset-id *synsets*))
         (lfilename (lexfile-name (synset-lnum synset)))
         (lemma (getf s :lemma))
         (words (synset-words synset)))
    (setf (gethash key *senses*)
          (list lfilename 
                (first (find lemma words :key #'first :test #'string-equal))
                (second (find lemma words :key #'first :test #'string-equal))
                key
                (getf s :synset)
                (getf s :ss-type)
                (position lemma words :key #'first :test #'string-equal)))))


(defun order-synsets (synset-ids)
  (flet ((synset-key (id)
           (format nil "~{~a~}" (mapcar #'string-downcase
					(mapcar #'first (synset-words (gethash id *synsets*)))))))
   (sort synset-ids #'string< :key #'synset-key)))


(defun generate-documentation ()
  (with-open-file (s "namespaces.txt" :if-exists :supersede :direction :output)
    (dolist (entry *lex-filenum*)
      (format s "~{~6D ~18@A ~8@A~}~%" entry)))

  (with-open-file (s "links.txt" :if-exists :supersede :direction :output)
    (dolist (entry *pointers*)
      (format s "~a:~%" (fourth (assoc (car entry) *pos* :test #'equal)))
      (dolist (link (cdr entry))
        (format s "~{~3A ~6A ~A~}~%" link))
      (format s "~%"))))


(defun load-en (dict-dir)
  (setf *senses* (make-hash-table :test #'equal))
  (setf *synsets* (make-hash-table :test #'equal))

  (generate-documentation)

  (dolist (f '("data.noun" "data.verb" "data.adj" "data.adv"))
    (mapcar #'add-synset (parse-file (merge-pathnames dict-dir f) #'parse-data-line)))

  (mapcar (lambda (x) (fix-lex-ids (first x))) *lex-filenum*)

  (mapcar #'add-senseidx (parse-file (merge-pathnames dict-dir "index.sense") #'parser-senseidx))

  (with-open-file (s "pwn-3.0.mapping" :direction :output :if-exists :supersede)
    (dolist (sense (hash-table-values *senses*))
      (if (= 0 (third sense))
          (format s "~a:~a,~a,~a,~a~%" (first sense) (second sense) (fourth sense)
		  (fifth sense) (sixth sense))
          (format s "~a:~a~a,~a,~a,~a~%" (first sense) (second sense) (third sense)
		  (fourth sense) (fifth sense) (sixth sense)))))

  (read-morphosemantic-links "morphosemantic-links.csv")

  (let ((namespaces (make-hash-table :test #'equal)))
    (maphash (lambda (k v)
               (push k (gethash (lexfile-name (synset-lnum v)) namespaces)))
	     *synsets*)

    (mapc (lambda (k)
            (setf (gethash k namespaces) (order-synsets (gethash k namespaces))))
          (hash-table-keys namespaces))

    (maphash (lambda (k v)
               (with-open-file (out (format nil "~a.~a" k (if *org-mode* "org" "txt"))
				    :direction :output :if-exists :supersede)
                 (dolist (s v)
                   (mk-synset out s))))
	     namespaces))
  t)

(defun test ()
  (load-en #p"/home/fcbr/repos/wordnet/WordNet-3.0/dict/"))

