
(in-package #:wordnet-dsl)

(w:add-namespace "schema" "https://br.ibm.com/tkb/own-en/schema/")
(w:add-namespace "nomlex" "https://br.ibm.com/tkb/own-en/nomlex/")

(defparameter *synset-pos-types* '(("noun" . !schema:NounSynset)
                                   ("adj" . !schema:AdjectiveSynset)
                                   ("adjs" . !schema:AdjectiveSatelliteSynset)
                                   ("adv" . !schema:AdverbSynset)
                                   ("verb" . !schema:VerbSynset)))

(defparameter *wordsense-pos-types* '(("noun" . !schema:NounWordSense)
                                      ("adj" . !schema:AdjectiveWordSense)
                                      ("adjs" . !schema:AdjectiveSatelliteWordSense)
                                      ("adv" . !schema:AdverbWordSense)
                                      ("verb" . !schema:VerbWordSenses)))

(defparameter *pointers* '(("ant" . !schema:antonymOf)
                           ("dr" . !schema:classifiedByRegion)
                           ("ihyper" . !schema:instanceOf)
                           ("hyper" . !schema:hyponymOf)
                           ("ant" . !schema:antonymOf)
                           ("attr" . !schema:attribute)
                           ("cause" . !schema:causes)
                           ("da" . !schema:adverbPertainsTo)
                           ("drf" . !schema:derivationallyRelated)
                           ("dt" . !schema:classifiedByTopic)
                           ("du" . !schema:classifiedByUsage)
                           ("entail" . !schema:entails)
                           ("frame" . !schema:frame)
                           ("hm" . !schema:memberHolonymOf)
                           ("hp" . !schema:partHolonymOf)
                           ("hs" . !schema:substanceHolonymOf)
                           ("pe" .  !schema:adjectivePertainsTo)
                           ("pv" . !schema:participleOf)
                           ("see" . !schema:seeAlso)
                           ("sim" . !schema:similarTo)
                           ("vg" . !schema:sameVerbGroupAs)
                           ("agent" . !nomlex:agent)
                           ("body-part" . !nomlex:bodyPart)
                           ("by-means-of" . !nomlex:byMeansOf)
                           ("destination" . !nomlex:destination)
                           ("event" . !nomlex:event)
                           ("instrument" . !nomlex:instrument)
                           ("location" . !nomlex:location)
                           ("material" . !nomlex:material)
                           ("property" . !nomlex:property)
                           ("result" . !nomlex:result)
                           ("state" . !nomlex:state)
                           ("undergoer" . !nomlex:undergoer)
                           ("uses" . !nomlex:uses)
                           ("vehicle" . !nomlex:vehicle)))

(defun escape-iri (str)
  (substitute #\_ #\' (substitute #\_ #\" (substitute #\- #\: str))))

(defun make-synset-iri (synset-id)
  (w:node (format nil "https://br.ibm.com/tkb/own-en/instances/synset-~a" (escape-iri synset-id))))

(defun make-wordsense-iri (wordsense-id)
  (w:node (format nil "https://br.ibm.com/tkb/own-en/instances/wordsense-~a" (escape-iri wordsense-id))))

(defun make-word-iri (word)
  (w:node (format nil "https://br.ibm.com/tkb/own-en/instances/word-~a" (sxhash (concatenate 'string word (string-downcase word))))))

(defun make-synset-type (synset)
  (cdr (assoc (first (split-sequence #\. (synset-file synset))) *synset-pos-types* :test #'equal)))

(defun make-wordsense-type (synset)
  (cdr (assoc (first (split-sequence #\. (synset-file synset))) *wordsense-pos-types* :test #'equal)))

(defun make-pointer (pointer)
  (cdr (assoc pointer *pointers* :test #'equal)))

(defun synset-semantic-pointers (synset)
  (remove-if-not (lambda (x) (equal 0 x)) (synset-pointers synset) :key #'car))

(defun synset-lexical-pointers (synset)
  (remove-if (lambda (x) (equal 0 x)) (synset-pointers synset) :key #'car))

(defun export-synset (synset-id synset)
  (let ((synset-iri (make-synset-iri synset-id))
        (synset-type (make-synset-type synset)))
    (w:add-triple (w:triple synset-iri !rdf:type synset-type))
    (w:add-triple (w:triple synset-iri !schema:lexicographerFile (literal (synset-file synset))))
    (w:add-triple (w:triple synset-iri !schema:synsetId (literal synset-id)))
    (add-wordsenses-triples synset-iri (make-wordsense-type synset) (synset-senses synset))
    (when (synset-gloss synset)
      (w:add-triple (w:triple synset-iri !schema:gloss (literal (synset-gloss synset)))))
    (dolist (p (synset-lexical-pointers synset))
      (unless (make-pointer (second p))
        (format t "Unknown lexical pointer: ~a~%" (second p)))
      (w:add-triple (w:triple (make-wordsense-iri (first p))
                                        (make-pointer (second p))
                                        (make-wordsense-iri (third p)))))
    (dolist (p (synset-semantic-pointers synset))
      (unless (make-pointer (second p))
        (format t "Unknown semantic pointer: ~a~%" (second p)))
      (w:add-triple (w:triple synset-iri (make-pointer (second p)) (make-synset-iri (third p)))))))

(defun add-wordsenses-triples (synset-iri wordsense-type senses)
  (dolist (s senses)
    (add-wordsense-triples synset-iri wordsense-type s)))

(defun add-wordsense-triples (synset-iri wordsense-type sense)
  (let ((word-iri (make-word-iri (cdr sense)))
        (wordsense-iri (make-wordsense-iri (car sense))))
    (w:add-triple (w:triple synset-iri !schema:containsWordSense wordsense-iri))
    (w:add-triple (w:triple wordsense-iri !rdf:type wordsense-type))
    (w:add-triple (w:triple wordsense-iri !schema:word word-iri))
    (w:add-triple (w:triple wordsense-iri !schema:senseKey (literal (car sense))))
    (w:add-triple (w:triple word-iri !rdf:type !schema:Word))
    (w:add-triple (w:triple word-iri !schema:lexicalForm (literal (cdr sense))))
    (w:add-triple (w:triple word-iri !schema:lemma (literal (string-downcase (cdr sense)))))))

;; synset -> schema:containsWordSense WS
;; WS a schema:WordSense
;; WS schema:word W
;; W a schema:Word
;; W wn30:lexicalForm/wn30:lemma W

(defun default-sense (synset)
  (caar (reverse (synset-senses synset))))

(defun export-synsets (wn)
  (dolist (s (remove-duplicates (mapcar #'car (hash-table-values wn))))
    (export-synset (default-sense s) s)))

(defun export-rdf (out)
  (let ((*db* (make-instance 'w::fast-temporary-db))
        (wn (read-wn #p"../dict/*.txt")))
    (export-synsets wn)
    (with-open-file (stream out :direction :output :if-exists :supersede)
      (w::dump-as-ntriples (db-triples *db*) stream))))


;; converting to ukb

(defun ukb-concept-id (sense-id)
  (destructuring-bind (fn localid)
      (cl-ppcre:split ":" sense-id)
    (destructuring-bind (pos lexname)
	(cl-ppcre:split "\\." fn)
      (format nil "~a/~a-~a" fn localid (cdr (assoc pos *pos* :test #'equal))))))


(defun synset-to-ukb (ss stream)
  (let ((default-sense (ukb-concept-id (caar (synset-senses ss)))))
    (dolist (p (synset-pointers ss))
      (format stream "u:~a v:~a d:0 w:1 s:own-en t:~a~%"
	      (if (equal 0 (car p)) default-sense (ukb-concept-id (car p)))
	      (ukb-concept-id (caddr p))
	      (cadr p)))
    (dolist (s (cdr (synset-senses ss)))
      (format stream "u:~a v:~a d:0 w:1 s:own-en t:syn~%"
	      default-sense
	      (ukb-concept-id (car s))))))


(defun convert-ukb (idx dict-file kb-file)
  (with-open-files ((sdt dict-file :direction :output :if-exists :supersede)
		    (skb   kb-file :direction :output :if-exists :supersede))
    (maphash (lambda (k v)
               (unless (car v)
                 (format t "invalid link: ~a: ~a~%" k v))) idx)
    (dolist (v (remove-duplicates (mapcar #'car (alexandria:hash-table-values idx))))
      (synset-to-ukb v skb))
    (maphash (lambda (k v)
	       (format sdt "~a ~{~a:1~^ ~}~%" k (mapcar #'ukb-concept-id v)))
	     (by-lemma idx))))
