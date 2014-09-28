(in-package :sonkv)

(defun %mkit () (make-instance 'lazy-bucket :id "some"))

(define-test lazy-bucket-not-allocated
    (assert-equal nil (allocated (%mkit))))

(define-test lazy-bucket-active
    (assert-equal t (active (%mkit))))

(define-test lazy-bucket-default-size
    (assert-equal +default-bucket-size+ (size (%mkit))))

(define-test lazy-bucket-index-query-unindex
    (let ((b (%mkit)) (& 0) ($ "Hello!"))
      (index b & $)
      (assert-equal $ (query b &))
      (unindex b &)
      (assert-equal nil (query b &))))

(define-test lazy-bucket-out-of-bounds
    (let ((b (%mkit)))
      (assert-error 'error (index b 8948941561 "hi!"))
      (assert-error 'simple-condition (query b 8948941561))
      (assert-error 'error (unindex b 8948941561)))
  (assert-equal nil (query (%mkit) 8948941561)))
