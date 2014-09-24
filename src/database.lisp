(in-package :sonkv)

(defconstant +default-modulus+ (expt 2 8))

(defgeneric db-set (database key value))
(defgeneric db-get (database key))
(defgeneric db-add (database key value))
(defgeneric db-sub (database key value))
(defgeneric db-mul (database key value))
(defgeneric db-div (database key value))

(defclass database ()
  ((modulus :type (integer 2 *) :initform +default-modulus+ :initarg :modulus
	    :reader modulus)
   (bucket  :type bucket :initform (error "bucket must be provided") :initarg :bucket :reader bucket)
   (hash :initform #'murmurhash:murmurhash :initarg :hash :reader hash)
   (name :type string :initarg :name :initform (error "name must be provided") :reader name)))

(defun make-basic-database (name)
  (make-instance 'database :name name :bucket (make-list-bucket +default-modulus+)))
(defun %hash (db key)
  (funcall (hash db) key))

(defmethod db-set ((database database) (key string) value)
  (index (bucket database) (%hash database key) value))

(defmethod db-get ((database database) (key string))
  (query (bucket database) (%hash database key)))

(defmethod db-add ((database database) (key string) (value number))
  (let* ((bucket (bucket database)) (hash (%hash database key))
	 (cur-val (or (query bucket hash) 0)))
    (when (numberp cur-val) (index bucket hash (+ cur-val value)))))

(defmethod db-sub ((database database) (key string) (value number))
  (let* ((bucket (bucket database)) (hash (%hash database key))
	 (cur-val (or (query bucket hash) 0)))
    (when (numberp cur-val) (index bucket hash (- cur-val value)))))

(defmethod db-mul ((database database) (key string) (value number))
  (let* ((bucket (bucket database)) (hash (%hash database key))
	 (cur-val (or (query bucket hash) 0)))
    (when (numberp cur-val) (index bucket hash (* cur-val value)))))

(defmethod db-div ((database database) (key string) (value number))
  (unless (zerop value)
    (let* ((bucket (bucket database)) (hash (%hash database key))
	   (cur-val (or (query bucket hash) 0)))
      (when (numberp cur-val) (index bucket hash (/ cur-val value))))))

(defun benchmark-database (&key (times 100000))
  (time
   (let ((db (make-instance 'database :bucket (make-list-bucket 256) :name "benchmark-db")))
     (loop for i from 0 to times do (db-set db (format nil "some-~a" (random times)) (random 5000))))))
