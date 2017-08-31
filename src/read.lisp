(in-package #:cl-wn-org)

(defun read-wn (dir list-files)
  (let ((wn (make-hash-table :test #'equal)))
    (loop for file in list-files do
	  (setf (gethash (make-symbol file) wn)
		(read-file (merge-pathnames dir file))))
    wn))


(defun read-file (org-file)
  (let ((file (open org-file))
	(file-synsets (make-hash-table :test #'equal)))
    (load-synsets file file-synsets)
    (close file)
    file-synsets))


(defun load-synsets (file file-synsets &optional (lines '()))
  (let ((line (read-line file nil nil)))
    (cond ((equal line "")
	   (make-synset lines file-synsets) (load-synsets file file-synsets '()))
	  
	  (line
	   (load-synsets file file-synsets (append lines (list line))))

	  ((not (null lines))
	   (make-synset lines file-synsets)))))


(defun make-synset (lines file-synsets)
  (let ((syn (make-instance 'synset)))
    (add-properties lines syn)
    (setf (gethash (synset-w syn) file-synsets)
	  syn)
    (setf (gethash 'all file-synsets)
	  (append (gethash 'all file-synsets) (list  (synset-w syn))))))


(defun add-properties (lines syn)
  (mapcar #'(lambda (x) (add x syn)) lines)
  (setf (synset-w syn) (sense-word (car (synset-senses syn)))))


(defun add (line syn)
  (let ((infos (cl-ppcre:split #\space line)))
    (cond ((equal "w:" (car infos))        (add-w syn infos))
	  ((equal "g:" (car infos))        (add-g syn line))
	  (T                               (add-rest syn infos))
	  )))

(defun add-w (syn infos)
  (let* ((word-dirty (nth 1 infos))
	 (word (clean-word word-dirty))
	 (sense (make-instance 'sense :word  word)))
    (setf (synset-senses syn)
	  (append (synset-senses syn)
		  (list sense)))
    (add-w-rest sense (cddr infos))))

(defun clean-word (word)
  (if (cl-ppcre:scan "[_a-z]*[a-z][1-9]$" word)
      (aref (nth 1 (multiple-value-list (cl-ppcre:scan-to-strings  "(.*)[1-9]$" word))) 0)
      word))
    

(defun add-w-rest (sense infos)
  (let ((link (car infos))
	(target (cadr infos)))
    (if (null link)
	nil
	(progn (setf (sense-links-targets sense)
		     (append (sense-links-targets sense)
			     (list link target)))
	       (add-w-rest sense (cddr infos))))))
 
(defun add-g (syn line)
  (setf (synset-gloss syn)
	(aref (nth 1 (multiple-value-list (cl-ppcre:scan-to-strings  ": (.*)" line))) 0)))

(defun add-rest (syn infos)
  (let ((link (car infos))
	(target  (cadr infos)))
     (setf (synset-pointers syn)
	  (append (synset-pointers syn)
		  (list link target)))
    (setf (synset-slot-pointers syn)
	  (append (synset-slot-pointers syn)
		  (list link)))))

(defun format-link (string)
  (substitute #\> #\] (substitute #\< #\[ string )))
