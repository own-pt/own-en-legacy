
(in-package #:wordnet-dsl)

(add-namespace "schema" "https://w3id.org/own-en/schema/")

(defparameter *pos-types* '(("noun" . !schema:NounSynset)
                            ("adj" . !schema:AdjectiveSynset)
                            ("adjs" . !schema:AdjectiveSatelliteSynset)
                            ("adv" . !schema:AdverbSynset)
                            ("verb" . !schema:VerbSynset)))

(defun make-synset-iri (synset-id)
  (node (format nil "https://w3id.org/own-en/instances/synset/~a" (substitute #\/ #\: synset-id))))

(defun make-synset-type (synset)
  (cdr (assoc (first (split-sequence #\. (synset-file synset))) *pos-types* :test #'equal)))

(defun export-synset (synset-id synset)
  (let ((synset-iri (make-synset-iri synset-id))
        (synset-type (make-synset-type synset)))
    (wilbur:add-triple (wilbur:triple synset-iri !rdf:type synset-type))))

(defun export-synsets (wn)
  (maphash (lambda (synset-id synset)
             (export-synset synset-id (first synset))) wn))

(defun export-rdf ()
  (let ((*db* (make-instance 'db))
        (wn (read-wn #p"../dict/*.txt")))
    (export-synsets wn)
    (with-open-file (stream "out.rdf" :direction :output :if-exists :supersede)
      (wilbur::dump-as-rdf/xml (db-triples *db*) stream (wilbur:namespaces)))))
