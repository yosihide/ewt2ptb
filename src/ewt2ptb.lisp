(declaim (optimize (speed 3) (space 0) (compilation-speed 0)))

(in-package :cl-user)

(defpackage :ewt2ptb
  (:use :cl
	:cl-ppcre
	:ewt2ptb.util
	:ewt2ptb.label
	:ewt2ptb.node)
  (:export :ewt->ptb))

(in-package :ewt2ptb)

(defun replace-empty-type! (tree)
  (dolist (n (preorder tree))
    (when (cat= n "-NONE-")
      (cond
	((string= (word n) "*0*")
	 (setf (word n) "0"))
	((scan "\\*PRO\\*" (word n))
	 (setf (word n) (regex-replace "\\*PRO\\*" (word n) "*")))
	)))
  tree)

(defun empty-element-type (node)
  (when (and (typep node 'internal-node)
	     (= (length (children node)) 1)
	     (cat= (node-ref node 0) "-NONE-"))
    (word (node-ref node 0))))

(defun trace? (node)
  (awhen (empty-element-type node)
    (find it '("*" "*EXP*" "*ICH*" "*PRO*" "*RNR*" "*T*") :test #'string=)))

(defun convert-ewt! (tree)
  (let ((parent-tab (make-hash-table :test 'eq))
	(left-tab (make-hash-table :test 'eq))
	(right-tab (make-hash-table :test 'eq))
	(id=>nodes (make-hash-table)))
    (labels
	((parent (node)
	   (gethash node parent-tab))
	 (left (node)
	   (gethash node left-tab))
	 (right (node)
	   (gethash node right-tab))
	 (get-nodes (id)
	   (gethash id id=>nodes))
	 (get-ids nil
	   (hash-keys id=>nodes))
	 (traverse (node parent)
	   (setf (gethash node parent-tab) parent)
	   (awhen (id node)
	     (push node (gethash it id=>nodes)))
	   (when (typep node 'internal-node)
	     (dolist (n (children node))
	       (traverse n node))
	     (loop
		:for n0 :in (cons nil (butlast (children node)))
		:for n1 :in (children node)
		:for n2 :in (append1 (rest (children node)) nil)
		:do
		  (setf (gethash n1 left-tab) n0)
		  (setf (gethash n1 right-tab) n2))))
	 (seek-nodes (node id)
	   (let ((parent (parent node))
		 (result nil))
	     (when (null parent)
	       (return-from seek-nodes nil))
	     (loop
		:for n := (left parent) :then (left n)
		:until (null n)
		:do
		  (when (equal id (id n))
		    (push n result)))
	     (loop
		:for n := (right parent) :then (right n)
		:until (null n)
		:do
		  (when (equal id (id n))
		    (push n result)))
	     (append result (seek-nodes parent id)))))
      (traverse tree nil)
      (let ((empty-element=>filler (make-hash-table :test 'eq))
	    (err 0))
	(dolist (id (get-ids))
	  (let ((nodes (get-nodes id)))
	    (cond
	      ;; annotation error
	      ((= (length nodes) 1)
	       (incf err))
	      ((>= (length (remove-if #'trace? nodes)) 2)
	       (incf err))
	      (t
	       ;; 1st step
	       (dolist (n nodes)
		 (cond
		   ;; *EXP*, *ICH* and *RNR*
		   ((find (empty-element-type n) '("*EXP*" "*ICH*" "*RNR*") :test #'string=)
		    (let ((candidates (remove-if #'empty-element-type nodes)))
		      (when (= (length candidates) 1)
			(setf (gethash n empty-element=>filler) (first candidates)))))
		   ;; *T*
		   ((string= (empty-element-type n) "*T*")
		    (let ((candidates (remove-if-not #'(lambda (x) (or (null (empty-element-type x)) (string= (empty-element-type x) "*0*"))) nodes)))
		      (when (= (length candidates) 1)
			(setf (gethash n empty-element=>filler) (first candidates)))))
		   ;; * and *PRO*
		   ((find (empty-element-type n) '("*" "*PRO*") :test #'string=)
		    (let ((candidates (seek-nodes n id)))
		      (awhen (or (find-if #'(lambda (x) (find "SBJ" (label-functions (node-label x)) :test #'string=)) candidates)
				 (first candidates))
			(setf (gethash n empty-element=>filler) it))))))

	       ;; 2nd step
	       (let* ((nodes (remove-if #'(lambda (x) (or (find x (hash-keys empty-element=>filler) :test #'eq)
							  (find x (hash-values empty-element=>filler) :test #'eq)))
					nodes))
		      (filler (find-if-not #'trace? nodes))
		      (empty-elements (remove-if-not #'trace? nodes)))

		 (when (and filler empty-elements)
		   (dolist (e empty-elements)
		     (setf (gethash e empty-element=>filler) filler))))))))

	(let ((filler-tab (make-hash-table :test 'eq))
	      (nodes (preorder tree)))
	  (dolist (f (hash-values empty-element=>filler))
	    (setf (gethash f filler-tab) t))
	  (loop
	     :for i :from 1
	     :for n :in nodes
	     :do
	       (cond
		 ((gethash n filler-tab)
		  (setf (label--index (node-label n)) i))
		 (t
		  (setf (label--index (node-label n)) nil))))
	  (dolist (n nodes)
	    (awhen (gethash n empty-element=>filler)
	      (setf (word (node-ref n 0))
		    (format nil "~a-~a" (word (node-ref n 0)) (label--index (node-label it)))))))

	;; gapping
	(let ((id=>gap-id (make-hash-table)))
	  (loop
	     :for i :from 1
	     :for n :in (preorder tree)
	     :do
	       (awhen (label-=index (node-label n))
		 (cond
		   ((null (gethash it id=>gap-id))
		    (setf (label-=index (node-label n)) nil)
		    (setf (gethash it id=>gap-id) i)
		    (setf (label--index (node-label n)) i))
		   (t
		    (setf (label-=index (node-label n)) (gethash it id=>gap-id)))))))

	(values (replace-empty-type! tree) err)))))

(defun ewt->ptb (&key ewt ptb)
  (write-trees (mapcar #'convert-ewt! (read-trees ewt))
	       ptb))
