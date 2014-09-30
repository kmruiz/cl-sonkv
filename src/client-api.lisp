(in-package :sonkv)

(defstruct socket-state database)

(defparameter *databases* (make-hash-table :test 'equal))
(defparameter *current-socket* nil)
(defparameter *js-lib* (empty-lib "js-api"))

(defun /make-database (name bucket)
  (setf (gethash name *databases*) (make-instance 'database :bucket (make-list-bucket bucket) :name name)))

(defun /select-database (name)
  (let ((db (gethash name *databases*)))
    (if (null db)
	nil
	(progn (setf (socket-state-database (socket-data *current-socket*)) db) t))))

(/make-database "default" 16)

(defun /get (i) (db-get (socket-state-database (socket-data *current-socket*)) i))
(defun /set (i v) (db-set (socket-state-database (socket-data *current-socket*)) i v))
(defun /rem (i) (/set i nil))
(defun /add (i v) (db-add (socket-state-database (socket-data *current-socket*)) i v))
(defun /sub (i v) (db-sub (socket-state-database (socket-data *current-socket*)) i v))
(defun /mul (i v) (db-mul (socket-state-database (socket-data *current-socket*)) i v))
(defun /div (i v)
  (unless (= v 0)
    (db-div (socket-state-database (socket-data *current-socket*)) i v)))

;; JAVASCRIPT API
(defun js-to-lisp (js-result)
  (let ((type (type-of js-result)))
    (cond
      ((subtypep type 'number) js-result)
      ((subtypep type 'string) js-result)
      ((string= (string type) "OBJ")
       (let ((k (make-hash-table :test 'equal)))
	 (js-for-in js-result
		    (lambda (x)
		      (setf (gethash (string x) k)
			    (js-to-lisp (js-prop js-result x)))))
	 k))
      ((string= (string type) "AOBJ")
       (let ((k nil)) (dotimes (i (js-array-length js-result)) (push (js-aref js-result i) k)) (reverse k)))
      (t nil))))

(add-to-lib *js-lib*
  (.object "database"
    (.active "list"
      (:read ()
	     (let ((data (loop for key being the hash-keys of *databases* collect key)))
	       (js-array (make-array (list (length data)) :initial-contents data :adjustable t)))))
    (.active "current"
      (:read () (name  (socket-state-database (socket-data *current-socket*))))))
  (.func "create" (database bucket-size)
    (/make-database (to-string database) (to-integer bucket-size))
    database)
  (.func "select" (database)
    (/select-database (to-string database)))
  (.func "get" (index)
    (/get (js-to-lisp index)))
  (.func "set" (index value)
    (/set (js-to-lisp index) (js-to-lisp value)))
  (.func "rem" (index)
    (/rem (js-to-lisp index)))
  (.func "add" (index number)
    (/add (js-to-lisp index) (js-to-lisp number)))
  (.func "sub" (index number)
    (/sub (js-to-lisp index) (js-to-lisp number)))
  (.func "mul" (index number)
    (/mul (js-to-lisp index) (js-to-lisp number)))
  (.func "div" (index number)
    (/div (js-to-lisp index) (js-to-lisp number))))
