(in-package :cl-wn-org)

(defun txt->rdf (wn new-file)
  (with-open-file (stream new-file :direction :output :if-exists :supersede ;:append
			  :if-does-not-exist :create)
    (maphash #'(lambda (file-name synsets) (synsets->rdf stream file-name synsets)) wn)))

(defun synsets->rdf (stream file-name synsets)
  (maphash #'(lambda (synset-id synset-obj)
	       (if (not (listp synset-obj)) (synset->rdf stream file-name synset-id synset-obj)))
	   synsets))

(defun synset->rdf (stream file-name synset-id synset-obj)
  (let ((synset (instance-synset stream file-name synset-id))
	(senses (synset-senses synset-obj)))
    (loop for sense in senses do
	  (add-wordsenses stream synset file-name sense)
	  (add-wordsenses-props ))))

(defun add-synset-type (stream synset file-name)
  (let ((file (symbol-name file-name)))
    (cond ((cl-ppcre:scan "NOUN" file) (add-type stream synset "NounSynset"))
	  ((cl-ppcre:scan "ADJ" file)  (add-type stream synset "AdjectiveSynset"))
	  ((cl-ppcre:scan "ADV" file)  (add-type stream synset "AdverbSynset"))
	  ((cl-ppcre:scan "VERB" file) (add-type stream synset "VerbSynset")))))

(defun add-type (stream synset type)
  (format stream "~a <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <https://w3id.org/own-pt/wn30/schema/~a> .~%" synset type))

(defun add-wordsenses (stream synset file-name sense)
  (let* ((word (sense-word sense))
	 (uri-word (format nil "<http://http://openwordnet-pt.org/../instances/wordsense-~a-~a>" file-name word)))
    (format stream "~a <https://w3id.org/own-pt/wn30/schema/containsWordSense> ~a .~%" synset uri-word)))

(defun instance-synset (stream file-name synset-id)
  (let ((synset (format nil "<http://http://openwordnet-pt.org/../instances/synset-~a-~a>" file-name synset-id)))
    (add-synset-type stream synset file-name)
    synset))
