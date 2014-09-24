(in-package :sonkv)

(defparameter *configuration-dir* (make-pathname :directory '(:absolute "etc" "sonkv" "conf.d") :name :wild :type "lisp"))

;; INITIALIZE CONFIGURATION
(loop for cfg in (directory *configuration-dir*)
   do (with-open-file (stream cfg)
	(loop for expr = (read stream nil nil) then (read stream nil nil)
	   unless (null expr)
	   do (eval expr))))

;; CONFIGURE DEFAULT VARIABLES
(defparameter *default-configuration*
  '(
;;; SERVER BASIC CONFIGURATION
    (+instance-name+ (machine-instance))
    (+pid-file+ #p"/etc/sonkv/PID")
    (+thread-count+ 2)
;;; NETWORK CONFIGURATION
    (+bind-ip+ (list "127.0.0.1"))
    (+port+ 43650)
    (+max-incomming-connections 128)
    (+backlog+ 32)
;;; AUTHENTICATION CONFIGURATION
    (+use-ssl+ nil)
    (+ssl-certificate+ nil)
    (+ssl-key+ nil)
    (+ssl-password+ nil)
;;;  LOG CONFIGURATION
    (+log-path-pattern+ "/var/log/sonkv/~a-sonkv.log")
    (+group-log-by+ :date) ;; :date or :pid
;;; STORAGE CONFIGURATION
    (+database-journal+ t)
    (+database-path+ #p"/etc/sonkv/data/")
    (+database-compress+ :zip)
;;; SCALABILITY
    (+broadcast-ip+ "255.255.255.255")
    (+scalability-policy+ :sharding))) ;; :sharding or :replication

(loop for i in *default-configuration*
   unless (boundp (car i))
   do (progn
	(eval `(defconstant ,(car i) ,(cadr i)))))

(defconstant +version+ "0.0.1b")

(format t "
#                    _
#                   | | ~a on ~a ~a ~a
#    ___  ___  _ __ | | ____   __
#   / __|/ _ \| '_ \| |/ /\ \ / /
#   \__ \ (_) | | | |   <  \ V /
#   |___/\___/|_| |_|_|\_\  \_/ @ ~a:~a
# v. ~a
#######################################"
	(lisp-implementation-type)
	(software-type)
	(machine-type)
	(software-version)
	+instance-name+
	+port+
	+version+)

(defun start-server ()
  (as:tcp-server nil +port+
		 (lambda (sock data)
		   ;; echo data back to client
		   (format t "~a~%" data)
		   (as:write-socket-data sock (format nil "response -> ~a~%" data)))
		 (lambda (ev)
		   (format t "ev: ~a~%" ev)))
  (as:signal-handler as:+sigint+
		     (lambda (sig)
		       (declare (ignore sig))
		       (as:exit-event-loop))))
