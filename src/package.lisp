(defun ql-load-required ()
  (ql:quickload '(bordeaux-threads cl-murmurhash cl-async)))

(ql-load-required)

(defpackage :sonkv
  (:use :common-lisp :bordeaux-threads :cl-murmurhash :cl-async))
