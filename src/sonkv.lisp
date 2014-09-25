(in-package :sonkv)

(defstruct socket-state database-name)

(defparameter *tcp-server* nil)

(defun start-server ()
  (print-logo)
  (setq *tcp-server*
	(as:tcp-server nil +port+
		       (lambda (sock data)
			 ;; data is a utf-8 encoded byte-array
			 (let ((cmd (octets-to-string data :encoding :utf-8)))
			   ;; as an example, ping back the request
			   (as:write-socket-data sock (format nil "ping back :> ~a~%" cmd))))
		       (lambda (ev)
			 (format t "ev: ~a~%" ev))))
  (as:signal-handler as:+sigint+
		     (lambda (sig)
		       (declare (ignore sig))
		       (as:exit-event-loop))))

(defun main ()
  (as:start-event-loop #'start-server))
