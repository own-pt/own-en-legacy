(in-package #:cl-wn-org)


(defparameter dicts '("adj.all.txt"             "adjs.all.txt"
		      "noun.animal.txt"		"noun.event.txt"
		      "noun.motive.txt"		"noun.process.txt"	"noun.time.txt"
		      "verb.consumption.txt"	"verb.possession.txt"   "adj.pert.txt"
		      "noun.artifact.txt"	"noun.feeling.txt"	"noun.object.txt"
		      "noun.quantity.txt"	"verb.body.txt"		"verb.contact.txt"
		      "verb.social.txt"         "adj.ppl.txt"		"noun.attribute.txt"
		      "noun.food.txt"		"noun.person.txt"      	"noun.relation.txt"
		      "verb.change.txt"		"verb.creation.txt"	"verb.stative.txt"
		      "adv.all.txt"		"noun.body.txt"
		      "noun.phenomenon.txt"	"noun.shape.txt"       	"verb.cognition.txt"
		      "verb.emotion.txt"   	"verb.weather.txt"      "noun.Tops.txt"
		      "noun.cognition.txt"	"noun.group.txt"	"noun.plant.txt"
		      "noun.state.txt"		"verb.communication.txt"      "verb.motion.txt"
		      "noun.act.txt"		"noun.communication.txt"      "noun.location.txt"
		      "noun.possession.txt"	"noun.substance.txt"	"verb.competition.txt"
		      "verb.perception.txt"))



(defun make-local-links (wn file)
  (let ((file-synsets (gethash file wn)))
    (loop for syn in (gethash 'all file-synsets) do
	  (local-links (gethash syn file-synsets) file-synsets))))

(defun local-links (syn file-synsets)
  (let ((slots (synset-slot-pointers syn))
	(pointers (synset-pointers syn)))
    (loop for slot in slots do
	  (add-local-link syn slot file-synsets (getf pointers slot) ))))

(defun add-local-link (syn prop file-synsets link-name)
    (if  (and
	  (stringp link-name)
	  (not (search "file:" link-name)))
      (let ((syn-link (gethash link-name file-synsets)))
	(if syn-link
	    (setf (getf (synset-pointers syn) prop) syn-link)
	    (format t "the link ~a from ~a synset dont exists"
		    link-name
		    (car (synset-words syn)))))))



 (defun make-external-links (wn list-files)
  (loop for file in list-files do
	(external-links-file wn file)))

(defun external-links-file (wn file)
  (let* ((file-synsets (gethash file wn))
	(synsets (gethash 'all file-synsets)))
    (loop for syn in synsets do
	  (add-external-links (gethash syn file-synsets) wn))))

(defun add-external-links (syn wn)
  (let ((prop-list (synset-slot-pointers syn))
	(links (synset-pointers syn)))
    (loop for prop in prop-list do
	  (add-external-links-aux syn wn (getf links prop) prop))))

(defun add-external-links-aux (syn wn link prop)
  (when (and (stringp link) (search "file:" link))
    (let* ((file-link (find-file link wn))
	   (syn-link-name (find-syn link))
	   (syn-link (gethash syn-link-name file-link)))
      (add-ext syn prop link file-link syn-link ))))




(defun find-file (link wn)
  (let* ((string-file (aref
		       (nth 1 (multiple-value-list (cl-ppcre:scan-to-strings ":(.*)::" link)))
		       0))
	 (symbol-file (find-symbol (string-upcase string-file))))
    (gethash symbol-file wn)))


(defun find-syn (link)
  (concatenate 'string "<<"
	       (aref
		(nth 1 (multiple-value-list (cl-ppcre:scan-to-strings "::(.*)" link)))
		0)))

(defun add-ext (syn prop link file-link syn-link )
  (if (or
	   (null file-link)
	   (null syn-link))
	  (format t "the link ~a  dont exists"
		       link)
      	  (setf (getf (synset-pointers syn) prop) syn-link)))
