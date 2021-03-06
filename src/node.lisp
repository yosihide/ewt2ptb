(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :cl-user)

(defpackage :ewt2ptb.node
  (:use :cl
	:cl-ppcre
	:ewt2ptb.util
	:ewt2ptb.label)
  (:export :make-node
	   :node-label
	   :cat
	   :cat=
	   :id
	   ;; internal node
	   :internal-node
	   :make-internal-node
	   :children
	   ;; terminal node
	   :terminal-node
	   :make-terminal-node
	   :word
	   ;; util
	   :node-ref
	   :preorder
	   ;; io
	   :read-tree
	   :read-trees
	   :write-tree
	   :write-trees
	   ))

(in-package :ewt2ptb.node)

(defstruct node
  label
  )

(defun cat (node)
  (label-category (node-label node)))

(defun cat= (node cat)
  (string= (cat node) cat))

(defun id (node)
  (label--index (node-label node)))

;; internal node
(defstruct (internal-node (:include node))
  children
  )

(defun children (node)
  (internal-node-children node))

;; terminal node
(defstruct (terminal-node (:include node))
  word
  )

(defun word (node)
  (terminal-node-word node))

(defun (setf word) (word node)
  (setf (terminal-node-word node) word))

;; util
(defmacro node-ref (node &rest indexes)
  (if (null indexes)
      node
      `(when (eq (type-of ,node) 'internal-node)
	 (node-ref (nth ,(car indexes) (children ,node)) ,@(cdr indexes)))))

(defun preorder (node)
  (case (type-of node)
    (terminal-node
     (list node))
    (internal-node
     (cons node (mapcan #'preorder (children node))))))

;;; reader
(defvar +eos+ (gensym))

(defun whitespace? (x)
  (member x '(#\Linefeed #\Newline #\Page #\Return #\Space #\Tab) :test #'char=))

(defun separator? (x)
  (or (char= x #\()
      (char= x #\))
      (whitespace? x)))

(defun make-char-buffer nil
  (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))

(defun push-char-buffer (x buf)
  (vector-push-extend x buf))

(defun get-char-buffer (buf)
  (let ((str (make-array (length buf) :element-type 'character)))
    (dotimes (i (length buf))
      (setf (char str i) (char buf i)))
    (setf (fill-pointer buf) 0)
    str))

(defun read-string (stream)
  (let ((buf (make-char-buffer)))
    (loop
       :while (not (separator? (peek-char nil stream)))
       :do
       (vector-push-extend (read-char stream) buf))
    (get-char-buffer buf)))

(defun expected-char (char stream &key skip?)
  (when (not (char= (peek-char t stream) char))
    (error "error: expected ~a~%" char))
  (when skip?
    (read-char stream)))

(defun unexpected-char (char stream)
  (when (char= (peek-char t stream) char)
    (error "error: unexpected ~a~%" char)))

(defun read-tree-aux (stream)

  (expected-char #\( stream :skip? t)

  (let ((root-label nil)
	(children nil)
	(result nil))

    ;; read label
    (unexpected-char #\) stream)

    (cond
      ((char= (peek-char t stream) #\()
       (setf root-label (string->label "TOP")))
      (t
       (setf root-label (string->label (read-string stream)))))

    (unexpected-char #\) stream)

    (cond
      ;; internal node
      ((char= (peek-char t stream) #\()
       (loop
	  :while (char= (peek-char t stream) #\()
	  :do
	  (push (read-tree-aux stream) children))
       (setf result
	     (make-internal-node :label root-label
				 :children (nreverse children))))

      ;; empty terminal
      ((string= (label-category root-label) "-NONE-")
       (destructuring-bind (type &optional id) (split "-" (read-string stream))
	 (when id
	   (setf (label--index root-label) (read-from-string id)))
	 (setf result (make-terminal-node :label root-label :word type))))

      ;; terminal
      (t
       (setf result (make-terminal-node :label root-label :word (read-string stream)))))

    (expected-char #\) stream :skip? t)

    result))

(defun read-tree (&optional (stream t) (eof-error-p t) eof-value)
  (if (and (not eof-error-p)
	   (eq (peek-char t stream nil +eos+) +eos+))
      (return-from read-tree eof-value)
      (read-tree-aux stream)))

(defmacro do-stream-tree ((x stream &optional result-form) &body body)
  (let ((gstrm (gensym)))
    `(let ((,gstrm ,stream))
      (do ((,x))
	  ((eq (setf ,x (read-tree ,gstrm nil +eos+)) +eos+) ,result-form)
	,@body))))

(defmacro do-file-tree ((x path &optional result-form) &body body)
  (let ((gstrm (gensym)))
    `(with-open-file (,gstrm ,path)
      (do-stream-tree (,x ,gstrm ,result-form) ,@body))))

(defun read-trees (file)
  (let ((trees nil))
    (do-file-tree (x file)
      (push x trees))
    (nreverse trees)))

;; writer
(defun write-tree (tree out)
  (let ((label (node-label tree)))
    (format out "(")
    (unless (cat= tree "TOP")
      (format out "~a" (label-category label)))
    (dolist (f (label-functions label))
      (format out "-~a" f))
    (case (type-of tree)
      (internal-node
       (awhen (label--index label)
	 (format out "-~a" it))
       (awhen (label-=index label)
	 (format out "=~a" it))
       (dolist (x (children tree))
	 (format out " ")
	 (write-tree x out)))
      (terminal-node
       (format out " ~a" (terminal-node-word tree))
       (when (label--index label)
	 (format out "-~a" (label--index label)))))
    (format out ")")))

(defun write-trees (trees file)
  (with-open-file (out file :direction :output)
    (dolist (tree trees)
      (write-tree tree out)
      (terpri out)))
  t)
