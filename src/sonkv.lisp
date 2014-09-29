(in-package :sonkv)

(defparameter *tcp-server* nil)


(defun start-server ()
  (print-logo)
  (setq *tcp-server*
	(as:tcp-server nil +port+
		       (lambda (sock data)
			 (when ())
			 ;; data is a utf-8 encoded byte-array
			 (let ((cmd (octets-to-string data :encoding :utf-8)))
			   (unwind-protect
				(progn ))
			   ;; as an example, ping back the request
			   (as:write-socket-data
			    sock
			    (format nil "~a~%"
				    (unwind-protect
					 (with-js-env (*js-lib*)
					   (setf *current-socket* sock)
					   (to-string (run-js cmd :optimize t)))
				      (setf *current-socket* nil))))))
		       (lambda (ev)
			 (format t "ev: ~a~%" ev))
		       :connect-cb
		       (lambda (sock)
			 (setf (socket-data sock) (make-socket-state :database (gethash "default" *databases*))))))
  (as:signal-handler as:+sigint+
		     (lambda (sig)
		       (declare (ignore sig))
		       (as:exit-event-loop))))

(defun main ()
  (as:start-event-loop #'start-server))
