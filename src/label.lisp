(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :cl-user)

(defpackage :ewt2ptb.label
  (:use :cl
	:cl-ppcre
	:ewt2ptb.util)
  (:export :make-label
	   :label-category
	   :label-functions
	   :label--index
	   :label-=index
	   :string->label))

(in-package :ewt2ptb.label)

(defstruct label
  category
  functions
  -index
  =index
  )

(defun extract-category (string)
  (regex-replace-all "(-[A-Z0-9]+|=[0-9]+)" string ""))

(defun extract-functions (string)
  (mapcar #'(lambda (x) (subseq x 1))
	  (all-matches-as-strings "-[A-Z]+" string)))

(defun extract-index (string)
  (awhen (nth-value 1 (scan-to-strings "(-)([0-9]+)" string))
    (read-from-string (svref it 1))))

(defun extract-=index (string)
  (awhen (nth-value 1 (scan-to-strings "(=)([0-9]+)" string))
	 (read-from-string (svref it 1))))

(defun string->label (string)
  (cond
    ((char= (char string 0) #\-)
     (make-label :category (scan-to-strings "^-[A-Z]+-" string)))
    (t
     (make-label :category (extract-category string)
		 :functions (extract-functions string)
		 :-index (extract-index string)
		 :=index (extract-=index string)))))
