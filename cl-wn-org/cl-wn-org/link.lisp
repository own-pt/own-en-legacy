(in-package #:cl-wn-org)



(defparameter dicts '(adj.all.org
		      noun.animal.org		noun.event.org
		      noun.motive.org		noun.process.org	noun.time.org
		      verb.consumption.org	verb.possession.org     adj.pert.org
		      noun.artifact.org	        noun.feeling.org	noun.object.org
		      noun.quantity.org	        verb.body.org		verb.contact.org
		      verb.social.org           adj.ppl.org		noun.attribute.org
		      noun.food.org		noun.person.org		noun.relation.org
		      verb.change.org		verb.creation.org	verb.stative.org
		      adv.all.org		noun.body.org		noun.geotime.org
		      noun.phenomenon.org	noun.shape.org		verb.cognition.org
		      verb.emotion.org   	verb.weather.org                noun.Tops.org
		      noun.cognition.org	noun.group.org		noun.plant.org
		      noun.state.org		verb.communication.org	verb.motion.org
		      noun.act.org		noun.communication.org	noun.location.org
		      noun.possession.org	noun.substance.org	verb.competition.org
		      verb.perception.org))


(defparameter *dic* '(lixo.org))

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
