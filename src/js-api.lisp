(in-package :sonkv)

(defstruct socket-state database)

(defparameter *databases* (make-hash-table))
(defparameter *current-socket* nil)
(defparameter *js-lib* (empty-lib "js-api"))

(defun make-database (name bucket)
  (setf (gethash name *databases*) (make-instance 'database :bucket (make-list-bucket bucket) :name name)))

(defun select-database (name)
  (let ((db (gethash name *databases*)))
    (if (null db)
	nil
	(progn (setf (socket-state-database (socket-data *current-socket)) db) t))))

(make-database "default" 16)

(add-to-lib *js-lib*
  (.object "database"
    (.active "list"
      (:read ()
	     (let ((data (loop for key being the hash-keys of *databases* collect key)))
	       (js-array (make-array (list (length data)) :initial-contents data :adjustable t)))))
    (.active "current"
      (:read () (name  (socket-state-database (socket-data *current-socket*))))))
  (.func "create" (database bucket-size)
    (make-database (to-string database) (to-integer bucket-size))
    database)
  (.func "select" (database)
    (select-database (to-string database)))
  (.func "get" (index)
    (db-get (socket-state-database (socket-data  *current-socket*)) (to-string index)))
  (.func "set" (index value)
    (db-set (socket-state-database (socket-data *current-socket*)) (to-string index) value))
  (.func "rem" (index)
    (db-set (socket-state-database (socket-data *current-socket*)) (to-string index) nil))
  (.func "add" (index number)
    (db-add (socket-state-database (socket-data *current-socket*)) (to-string index) (to-number number)))
  (.func "sub" (index number)
    (db-add (socket-state-database (socket-data *current-socket*)) (to-string index) (to-number number)))
  (.func "mul" (index number)
    (db-add (socket-state-database (socket-data *current-socket*)) (to-string index) (to-number number)))
  (.func "div" (index number)
    (unless (= (to-number number) 0)
      (db-add (socket-state-database (socket-data *current-socket*)) (to-string index) (to-number number)))))
