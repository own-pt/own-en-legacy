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
  (let ((synset-uri (make-uri "synset" file-name synset-id))
	(senses (synset-senses synset-obj)))
    (add-synset-type stream synset-uri file-name)
    (add-synset-props stream (symbol-name file-name) synset-uri synset-obj)
    (add-gloss stream synset-uri (synset-gloss synset-obj))
    (loop for sense in senses do
	  (add-wordsenses stream synset-uri file-name sense))))

(defun add-gloss (stream synset-uri gloss)
  (format stream "~a <https://w3id.org/own-pt/wn30/schema/gloss> ~s .~%" synset-uri gloss))


(defun make-uri (type file-name synset-id)
  (cond ((cl-ppcre:scan ":" synset-id)
	  (let ((file (aref (nth 1 (multiple-value-list (cl-ppcre:scan-to-strings  "(.*):" synset-id))) 0))
		(id (aref (nth 1 (multiple-value-list (cl-ppcre:scan-to-strings  ":(.*)" synset-id))) 0)))
	    (format nil "<https://w3id.org/own-pt/wn30-en/instances/~a-~a-~a>" type file id)))
	(t
	 (let ((file (aref (nth 1 (multiple-value-list (cl-ppcre:scan-to-strings "(.*).txt" (string-downcase file-name)))) 0)))
	    (format nil "<https://w3id.org/own-pt/wn30-en/instances/~a-~a-~a>" type file synset-id)))))


(defun add-synset-type (stream synset file-name)
  (let ((file (string-upcase (symbol-name file-name))))
    (cond ((cl-ppcre:scan "NOUN" file) (add-type stream synset "NounSynset"))
	  ((cl-ppcre:scan "ADJ" file)  (add-type stream synset "AdjectiveSynset"))
	  ((cl-ppcre:scan "ADV" file)  (add-type stream synset "AdverbSynset"))
	  ((cl-ppcre:scan "VERB" file) (add-type stream synset "VerbSynset")))))

(defun add-wordsenses (stream synset file-name sense)
  (let* ((word (sense-word sense))
	 (uri-sense (make-uri "wordsense" file-name word)))
    (format stream "~a <https://w3id.org/own-pt/wn30/schema/containsWordSense> ~a .~%" synset uri-sense)
    (add-type stream uri-sense "WordSense")
    (add-word stream (symbol-name file-name) uri-sense sense)
    (add-wordsense-props stream (symbol-name file-name) uri-sense sense)))

(defun add-word (stream file-name uri-sense sense-obj)
  (let* ((word (sense-word sense-obj))
	 (uri-word (make-uri "word" file-name word)))
    (format stream "~a  <https://w3id.org/own-pt/wn30/schema/Word> ~a .~%" uri-sense uri-word)
    (add-type stream uri-word "Word")
    (add-lexicalform stream uri-word word)))

(defun add-lexicalform (stream uri-word word)
  (let ((lexicalform  (clean-word word)))
    (format stream "~a <https://w3id.org/own-pt/wn30/schema/lexicalForm> \"~a\" .~%" uri-word lexicalform)))

(defun clean-word (word)
  (substitute #\space #\_ word))


(defun add-wordsense-props (stream file-name uri-sense sense-obj)
  (let ((pointers (cdr (assoc file-name  *pointers* :test #'(lambda (x y) (cl-ppcre:scan (string-upcase y) (string-upcase x)))))))
    (add-prop stream uri-sense (sense-links-targets sense-obj) pointers file-name "wordsense")))


(defun add-synset-props (stream file-name synset synset-obj)
  (let ((pointers (cdr (assoc file-name  *pointers* :test #'(lambda (x y) (cl-ppcre:scan (string-upcase y) (string-upcase x)))))))
    (add-prop stream synset (synset-pointers synset-obj) pointers file-name "synset")))


(defun add-prop (stream synset slot-pointers pointers file-name type)
  (when (not (null  slot-pointers))
    (let ((target (make-uri type file-name (cadr slot-pointers))))
	(format stream "~a ~a ~a . ~%"
		synset
		(cdr (assoc (car slot-pointers) pointers :test #'(lambda (x y) (cl-ppcre:scan y x))))
		target)
      (add-prop stream synset (cddr slot-pointers) pointers file-name type))))


(defun add-type (stream synset type)
  (format stream "~a <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <https://w3id.org/own-pt/wn30/schema/~a> .~%" synset type))


(defparameter *pointers* '(("NOUN" . (("ant" . "<https://w3id.org/own-pt/wn30/schema/antonymOf>")
				      ("hyper" . "<https://w3id.org/own-pt/wn30/schema/hypernymOf>")
				      ("ihyper" . "<https://w3id.org/own-pt/wn30/schema/instanceOf>")
				      ("hypo" . "<https://w3id.org/own-pt/wn30/schema/hyponymOf>")
					; ("ihypo" "Instance Hyponym") i dont find this in any synset
				      ("hm" . "<https://w3id.org/own-pt/wn30/schema/memberHolonymOf>")
				      ("hs" . "<https://w3id.org/own-pt/wn30/schema/substanceHolonymOf>")
				      ("hp" . "<https://w3id.org/own-pt/wn30/schema/partHolonymOf>")
				      ("mm" . "<https://w3id.org/own-pt/wn30/schema/memberMeronymOf>")
				      ("ms" . "<https://w3id.org/own-pt/wn30/schema/substanceHolonymOf>")
				      ("mp" . "<https://w3id.org/own-pt/wn30/schema/partMeronymOf>")
				      ("attr" . "<https://w3id.org/own-pt/wn30/schema/attribute>")
				      ("drf" . "<https://w3id.org/own-pt/wn30/schema/derivationallyRelated>")
				      ("dt" . "<https://w3id.org/own-pt/wn30/schema/classifiedByTopic>")
					; ("mt" "Member of this domain - TOPIC") i dont find this in any synset
				      ("dr" . "<https://w3id.org/own-pt/wn30/schema/classifiedByRegion>")
					;("mr" "Member of this domain - REGION") i dont find this in any synset
				      ("du" . "<https://w3id.org/own-pt/wn30/schema/classifiedByUsage>")
					;("mu" "Member of this domain - USAGE") i dont find this in any synset
				      ))
			   ("VERB" . (("ant" . "<https://w3id.org/own-pt/wn30/schema/antonymOf>")
				      ("hyper" . "<https://w3id.org/own-pt/wn30/schema/hypernymOf>")
				      ("hypo" . "<https://w3id.org/own-pt/wn30/schema/hyponymOf>")
				      ("entail" . "<https://w3id.org/own-pt/wn30/schema/entails>")
				      ("cause" . "<https://w3id.org/own-pt/wn30/schema/causes>")
				      ("see" . "<https://w3id.org/own-pt/wn30/schema/seeAlso>")
				      ("vg" . "<tps://w3id.org/own-pt/wn30/schema/sameVerbGroupAs>")
				      ("drf" . "<https://w3id.org/own-pt/wn30/schema/derivationallyRelated>")
				      ("dt" . "<https://w3id.org/own-pt/wn30/schema/classifiedByTopic>")
				      ("dr" . "<https://w3id.org/own-pt/wn30/schema/classifiedByRegion>")
				      ("du" . "<https://w3id.org/own-pt/wn30/schema/classifiedByUsage>")
				      ("frame" . "<https://w3id.org/own-pt/wn30/schema/frame>")))
			   ("ADJ." . (("ant" . "<https://w3id.org/own-pt/wn30/schema/antonymOf>")
				     ("sim" . "<https://w3id.org/own-pt/wn30/schema/similarTo>")
				     ("pv" . "<https://w3id.org/own-pt/wn30/schema/participleOf>")
					;("\\" "pe" "Pertainym (pertains to noun)")
				     ("attr" . "<https://w3id.org/own-pt/wn30/schema/attribute>")
				     ("see:" . "<https://w3id.org/own-pt/wn30/schema/seeAlso>")
				     ("drf" . "<https://w3id.org/own-pt/wn30/schema/derivationallyRelated>")
				     ("dt" . "<https://w3id.org/own-pt/wn30/schema/classifiedByTopic>")
				     ("dr" . "<https://w3id.org/own-pt/wn30/schema/classifiedByRegion>")
				     ("du" . "<https://w3id.org/own-pt/wn30/schema/classifiedByUsage>")))
			   ("ADJS" . (("ant" . "<https://w3id.org/own-pt/wn30/schema/antonymOf>")
				      ("sim" . "<https://w3id.org/own-pt/wn30/schema/similarTo>")
				      ("pv" . "<https://w3id.org/own-pt/wn30/schema/participleOf>")
					;("\\" "pe" "Pertainym (pertains to noun)")
				      ("attr" . "<https://w3id.org/own-pt/wn30/schema/attribute>")
				      ("see" . "<https://w3id.org/own-pt/wn30/schema/seeAlso>")
				      ("drf" . "<https://w3id.org/own-pt/wn30/schema/derivationallyRelated>")
				      ("dt" . "<https://w3id.org/own-pt/wn30/schema/classifiedByTopic>")
				      ("dr" . "<https://w3id.org/own-pt/wn30/schema/classifiedByRegion>")
				      ("du" . "<https://w3id.org/own-pt/wn30/schema/classifiedByUsage>")))
			   ("ADV" . (("ant" . "<https://w3id.org/own-pt/wn30/schema/antonymOf>")
				     ("da" . "<https://w3id.org/own-pt/wn30/schema/adverbPertainsTo>")
				     ("dt" . "<https://w3id.org/own-pt/wn30/schema/classifiedByTopic>")
				     ("dr" . "<https://w3id.org/own-pt/wn30/schema/classifiedByRegion>")
				     ("du" . "<https://w3id.org/own-pt/wn30/schema/classifiedByUsage>")))))
