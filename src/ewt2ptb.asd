(defpackage :ewt2ptb.system
  (:use :cl
	:asdf))

(in-package :ewt2ptb.system)

(defsystem "ewt2ptb"
  :depends-on (:cl-ppcre)
  :description "A tool for converting English Web Treebank into Penn Treebank format"
  :version "0.1"
  :author "Yoshihide Kato"
  :components ((:file "util")
	       (:file "label")
	       (:file "node")
	       (:file "ewt2ptb")))
