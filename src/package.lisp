(defpackage #:sonkv
  (:use #:common-lisp #:bordeaux-threads #:cl-murmurhash #:cl-async #:babel #:split-sequence #:lisp-unit)
  (:export
   ;; basic usage exports
   #:+version+
   #:main
   ;; testing function
   #:run-tests!
   ))
