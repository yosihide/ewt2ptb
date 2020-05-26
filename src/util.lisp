(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :cl-user)

(defpackage :ewt2ptb.util
  (:use :cl)
  (:export :aif
	   :awhen
	   :it
	   :append1
	   :hash-keys
	   :hash-values
	   ))

(in-package :ewt2ptb.util)

;; anaphoric macro
(defmacro aif (test then &optional else)
  `(let ((it ,test))
    (declare (ignorable it))
    (if it ,then ,else)))

(defmacro awhen (test &body body)
  `(aif ,test
    (progn ,@body)))

;; list
(declaim (inline append1))
(defun append1 (lst x) (append lst (list x)))

;; hash table
(defun hash-keys (hash)
  (let ((result nil))
    (maphash #'(lambda (key val) (declare (ignore val)) (push key result))
	     hash)
    result))

(defun hash-values (hash)
  (let ((result nil))
    (maphash #'(lambda (key val) (declare (ignore key)) (push val result))
	     hash)
    result))
