(in-package #:cl-wn-org)

(defun read-wn (dir list-files)
  (let ((wn (make-hash-table :test #'equal)))
    (loop for file in list-files do
	  (setf (gethash (make-symbol file) wn)
		(read-file (merge-pathnames dir file) file)))
    wn))


(defun read-file (org-file file-name)
  (let ((file (open org-file))
	(file-synsets (make-hash-table :test #'equal)))
    (load-synsets file file-name file-synsets)
    (close file)
    file-synsets))


(defun load-synsets (file file-name file-synsets &optional (lines '()))
  (let ((line (read-line file nil nil)))
    (cond ((equal line "")
	   (make-synset file-name lines file-synsets) (load-synsets file file-name file-synsets '()))
	  
	  (line
	   (load-synsets file file-name file-synsets (append lines (list line))))

	  ((not (null lines))
	   (make-synset file-name lines file-synsets)))))


(defun make-synset (file-name lines file-synsets)
  (let ((syn (make-instance 'synset :lex-file file-name)))
    (add-properties lines syn)
    (setf (gethash (synset-w syn) file-synsets)
	  syn)
    (setf (gethash 'all file-synsets)
	  (append (gethash 'all file-synsets) (list  (synset-w syn))))))


(defun add-properties (lines syn)
  (mapcar #'(lambda (x) (add x syn)) lines)
  (setf (synset-w syn) (sense-id (car (synset-senses syn)))))


(defun add (line syn)
  (let ((infos (cl-ppcre:split "\\s+" line)))
    (cond ((equal "w:" (car infos))        (add-w syn infos))
	  ((equal "g:" (car infos))        (add-g syn line))
	  (T                               (add-rest syn infos)))))

(defun add-w (syn infos)
  (let* ((word-dirty (nth 1 infos))
	 (word (clean-word-1 word-dirty))
	 (sense (make-instance 'sense :word  word :id word-dirty)))
    (setf (synset-senses syn)
	  (append (synset-senses syn)
		  (list sense)))
    (add-w-rest sense (cddr infos))))

(defun clean-word-1 (word)
   (if (cl-ppcre:scan "[_a-z]*[a-z][1-9]$" word)
       (aref (nth 1 (multiple-value-list (cl-ppcre:scan-to-strings  "(.*)[1-9]$" word))) 0)
       word))
    

(defun add-w-rest (sense infos)
  (if (null infos)
      nil
      (let ((link (car infos)))
	(if (equal link "frame")
	    (let* ((separated (separate-for-frame (cdr infos)))
		   (target (make-target-for-frame (car separated))))
	      (setf (sense-links-targets sense)
		    (append (sense-links-targets sense)
			    (list link target)))
	      (add-w-rest sense (cadr separated)))
	    (let ((target (cadr infos)))
	      (setf (sense-links-targets sense)
		    (append (sense-links-targets sense)
			    (list link target)))
	      (add-w-rest sense (cddr infos)))))))
 
(defun add-g (syn line)
  (setf (synset-gloss syn)
	(aref (nth 1 (multiple-value-list (cl-ppcre:scan-to-strings  ": (.*)" line))) 0)))

(defun add-rest (syn infos)
  (let ((link (car infos)))
    (if (equal link "frame")
	(let ((target (make-target-for-frame (cdr infos))))
	  (setf (synset-pointers syn)
		(append (synset-pointers syn)
			(list link target)))
	  (setf (synset-slot-pointers syn)
		(append (synset-slot-pointers syn)
		       (list link))))
	(let ((target  (cadr infos)))
	  (setf (synset-pointers syn)
		(append (synset-pointers syn)
			(list link target)))
	  (setf (synset-slot-pointers syn)
		(append (synset-slot-pointers syn)
			(list link)))))))

(defun separate-for-frame (list &optional (number-list '()))
  (if (and (not (null list)) (cl-ppcre:scan  "^[0-9]*$" (car list)))
      (separate-for-frame (cdr  list) (append number-list (list (car list))))
      (list number-list list)))

(defun make-target-for-frame (list)
  (reduce #'(lambda (x y) (concatenate 'string x "-" y)) list))

(defun format-link (string)
  (substitute #\> #\] (substitute #\< #\[ string )))
