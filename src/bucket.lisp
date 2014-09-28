(in-package :sonkv)

;; DEFAULT TREE-BUCKET SIZE IN ELEMENTS. WHEN THE BUCKET NEEDS TO ALLOCATE MORE MEMORY
;; IT RESERVES SOME LAZY-BUCKETS THAT ONLY ALLOCATES MEMORY WHEN THEY ARE ACCESSED THE
;; FIRST TIME.
(defconstant +default-bucket-size+ 256)

;;; GENERIC BUCKET METHODS
(defgeneric index (bucket index element)
  (:documentation "Inject the element at the selected index in the bucket. If the index is out of the
bucket bounds, it should signal a condition of type INDEX-OUT-OF-BUCKET-BOUNDS"))

(defmethod index :before (bucket index element)
  (assert (active bucket)))

(defgeneric query (bucket index)
  (:documentation "Gets the element at the selected index in the bucket. If the index is out of the bucket
bounds it should signal a condition of type INDEX-OUT-OF-BUCKET-BOUNDS. Accessing an unset index must return
nil. It must return a reference to the element, not a copy of it."))

(defmethod query :before (bucket index)
  (assert (active bucket)))

(defgeneric unindex (bucket index)
  (:documentation "Removes the element in the index of the bucket. Accessing an invalid index signals an
INDEX-OUT-OF-BOUNDS condition. Calling query on a removed element must return nil." ))

(defmethod unindex :before (bucket index)
  (assert (active bucket)))

;;; AN INTERFACE OF BUCKET, ALL BUCKETS MUST EXTEND THIS CLASS
(defclass bucket () nil)

;;; SINGLE-THREADED LAZY BUCKET IMPLEMENTATION
(defclass lazy-bucket (bucket)
  ((size :type (integer 2 +default-bucket-size+) :initform +default-bucket-size+ :initarg :size
	    :writer (setf size) :reader size)
   (data    :type array :initform (make-array nil) :initarg :data
	    :writer (setf data) :reader data)
   (active  :type :boolean :initform nil :writer (setf active) :reader active)
   (allocated :type boolean :initform nil :writer (setf allocated) :reader allocated)
   (id      :type string :initform (error "id must be set") :initarg :id
	    :reader id)))

(defun %initialize-if-needed (lazy-bucket)
  (unless (allocated lazy-bucket)
    (setf (data lazy-bucket) (make-array (size lazy-bucket) :initial-element nil))
    (setf (allocated lazy-bucket) t)))

(defmethod initialize-instance :after ((bucket lazy-bucket) &key)
  (setf (active bucket) t)
  bucket)

(defmethod index ((bucket lazy-bucket) (index number) element)
  (%initialize-if-needed bucket)
  (setf (aref (data bucket) index) element))

(defmethod query ((bucket lazy-bucket) (index number))
  (cond
    ((allocated bucket)
     (cond
       ((or (< index 0) (>= index (size bucket)))
	(signal "INDEX ~A OUT OF BOUNDS IN BUCKET OF SIZE ~A~%" index (size bucket)))
       (t (aref (data bucket) index))))
    (t nil)))

(defmethod unindex ((bucket lazy-bucket) (index number))
  (index bucket index nil))

;;; THREAD-SAFE BUCKET IMPLEMENTATION
;;; IT DECORATES ANOTHER BUCKET AND PROVIDES THREAD-SAFE ACCESS
;;; TO ITS ELEMENTS.
(defun %wait-lock (lock)
  (bordeaux-threads:acquire-lock lock) ;; waits until the lock is free but releases it immediately
  (bordeaux-threads:release-lock lock))

(defclass thread-safe-bucket (bucket)
  ((locks      :initform (make-hash-table :synchronized t)
	       :reader locks)
   (delegate   :initform (make-instance 'lazy-bucket) :initarg :delegate :reader delegate)))

(defmethod index ((bucket thread-safe-bucket) (index number) element)
  (let ((lock (gethash index (locks bucket))))
    (when (null lock)
      (setq lock (bordeaux-threads:make-lock
		  (format nil "mutex-for-bucket-~a-index-~a" (id (delegate bucket)) index)))
      (setf (gethash index (locks bucket)) lock))
    (bordeaux-threads:with-lock-held (lock)
      (index (delegate bucket) index element))))

(defmethod query ((bucket thread-safe-bucket) (index number))
  (let ((lock (gethash index (locks bucket))))
    (cond
      ((null lock) (query (delegate bucket) index))
      (t (%wait-lock lock)
       (query (delegate bucket) index)))))

(defmethod unindex ((bucket thread-safe-bucket) (index number))
  (let ((lock (gethash index (locks bucket))))
    (cond
      ((null lock) nil)
      (t (bordeaux-threads:with-lock-held (lock) (unindex (delegate bucket) index))))))


(defmethod active ((bucket thread-safe-bucket))
  (active (delegate bucket)))

;;; A LIST-BUCKET IS AN ARRAY OF LAZY BUCKETS THAT ARE ACCESSED BY MODULUS.
;;; LIST BUCKETS ARE RECURSIVE (THERE COULD BE SOME LIST-BUCKETS BEING CHILD OF
;;; ANOTHER ONE) LIKE A B-TREE. ACCESSING A LIST-BUCKET IS ONLY THREAD-SAFE WHEN
;;; IT'S INITIALIZED WITH THE THREAD-SAFE INITARG
(defclass list-bucket (bucket)
  ((bucket-map :initform nil :writer (setf bucket-map) :reader bucket-map)
   (modulus :type (integer 1 *) :initform (error "modulus must be set") :initarg :modulus :reader modulus)
   (thread-safe :type boolean :initform nil :initarg :thread-safe :reader thread-safe)))

(defmethod initialize-instance :after ((bucket list-bucket) &key)
  (setf (bucket-map bucket) (make-hash-table :synchronized (thread-safe bucket))))

(defun %put-child-basic-bucket (parent index)
  (let ((bucket (gethash index (bucket-map parent))))
    (when (null bucket)
      (let ((child-bucket (if (thread-safe parent)
			      (make-thread-safe-bucket (modulus parent))
			      (make-lazy-bucket (modulus parent)))))
	(setf (gethash index (bucket-map parent)) child-bucket)
	(setq bucket child-bucket)))
    bucket))

(defun %put-child-list-bucket (parent index)
  (let ((bucket (gethash index (bucket-map parent))))
    (when (null bucket)
      (let ((child-bucket (make-list-bucket (modulus parent) :thread-safe (thread-safe parent))))
	(setf (gethash index (bucket-map parent)) child-bucket)
	(setq bucket child-bucket)))
    bucket))

(defmethod index ((bucket list-bucket) (index number) element)
  (cond
    ((< index (modulus bucket))
     (index (%put-child-basic-bucket bucket 0) index element))
    (t
     (multiple-value-bind (div mod) (truncate index (modulus bucket))
       (index (%put-child-list-bucket bucket mod) div element)))))

(defmethod query ((bucket list-bucket) (index number))
  (cond
    ((< index (modulus bucket))
     (let ((child (gethash 0 (bucket-map bucket))))
       (unless (null child)
	 (query child index))))
    (t
     (multiple-value-bind (div mod) (truncate index (modulus bucket))
       (let ((child (gethash mod (bucket-map bucket))))
	 (unless (null child)
	   (query child div)))))))

(defmethod active ((bucket list-bucket)) t)

(defun make-lazy-bucket (mod)
  (make-instance 'lazy-bucket :size mod :id (format nil "unsafe-~a" mod)))

(defun make-thread-safe-bucket (mod)
  (make-instance 'thread-safe-bucket :delegate (make-lazy-bucket mod)))

(defun make-list-bucket (mod &optional &key (thread-safe t))
  (make-instance 'list-bucket :modulus mod :thread-safe thread-safe))

(defun bucket-benchmark-index (&optional &key (bucket-size 256) (bucket-factory #'make-list-bucket) (times 1000000))
  (format t "bucket-size: ~a | operation: ~a | factory: ~a | times: ~a~%"
	  bucket-size "index" bucket-factory times)
  (time
   (let ((bucket (funcall bucket-factory bucket-size)))
     (loop for i from 0 to times
	do (index bucket (random times) (format nil "Some value ~a" (random times)))))))

(defun %mt-time-work-fn (bucket times)
  (lambda ()
    (loop for i from 0 to times do (index bucket (random times) (format nil "Some value ~a" (random times))))))

(defun multithread-benchmark-index (&optional &key (threads 2) (bucket-size 256) (bucket-factory #'make-list-bucket) (times 1000000))
  (let ((bucket (funcall bucket-factory bucket-size)))
    (time
     (let ((k
	    (loop for i from 1 to threads
	       collect (bordeaux-threads:make-thread (%mt-time-work-fn bucket (round (/ times threads)))))))
       (loop for i in k do (bordeaux-threads:join-thread i))))))
