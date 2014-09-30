(in-package :sonkv)

(defparameter *tcp-server* nil)

(defun start-server ()
  (print-logo)
  (setq *tcp-server*
	(as:tcp-server nil +port+
		       (lambda (sock data)
			 ;; data is a utf-8 encoded byte-array
			 (let ((cmd (octets-to-string data :encoding :utf-8)))
			   (cond
			     ;; if the first char is an open paren, treat it as lisp
			     ((char= #\( (char cmd 0))
			      (unwind-protect
				   (progn
				     (setf *current-socket* sock)
				     (as:write-socket-data sock (format nil "~a~%" (eval (read-from-string cmd)))))
				(setf *current-socket* nil)))
			     ;; if not, it's just javascript
			     (t (as:write-socket-data
				 sock
				 (format nil "~a~%"
					 (unwind-protect
					      (with-js-env (*js-lib*)
						(setf *current-socket* sock)
						(cl-json:encode-json-to-string (js-to-lisp (run-js cmd :optimize t))))
					   (setf *current-socket* nil))))))))
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
  (in-package :sonkv)
  (as:start-event-loop #'start-server))
