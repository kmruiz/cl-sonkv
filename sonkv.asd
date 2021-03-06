;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:sonkv-asd
  (:use :cl :asdf))

(in-package :sonkv-asd)

(defsystem sonkv
  :name "sonkv"
  :version "0.0.1"
  :maintainer "Kevin Mas Ruiz"
  :author "Kevin Mas Ruiz <masruizkevin@gmail.com>"
  :licence "BSD sans advertising clause (see file COPYING for details)"
  :description "A key value store"
  :long-description "Lisp implementation of a persistent key-value store"
  :serial t
  :components ((:file "src/package")
	       (:file "src/configuration")
	       (:file "src/bucket")
	       (:file "src/database")
	       (:file "src/client-api")
	       (:file "src/sonkv"))
  :depends-on ("bordeaux-threads" "cl-murmurhash" "cl-async" "babel" "split-sequence" "lisp-unit" "cl-js" "cl-json"))
