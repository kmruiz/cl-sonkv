(require :asdf)

(let (%state)
  (defun require! (step)
    (if (not (member step %state :test #'eq))
	(progn
	  (push step %state)
	  (or (funcall step) (error "~a: COULD NOT ARCHIEVE GOAL" step)))
	t))

  (defun clean! ()
    (setf %state nil)))

(defun load-project ()
  (load "src/sonkv.asd" :verbose nil :print nil)
  (asdf:load-system :sonkv))

(defun test ()
  (when (require! #'load-project)
    (load "test/runner.lisp")
    (let ((d (make-pathname :directory '(:relative "test") :name :wild :type "lisp")))
      (mapcar (lambda (x) (unless (search "runner" (namestring x)) (load x))) (directory d))
      (funcall (symbol-function (find-symbol "RUN-TESTS!" :sonkv))))))

(defun build ()
  (when (require! #'test)
    (let ((version (eval (find-symbol "+VERSION+" :sonkv))))
      (ensure-directories-exist (format nil "build/~a/" version))
      (sb-ext:save-lisp-and-die
       (format nil "build/~a/sonkv" version)
       :executable t
       :toplevel (find-symbol "MAIN" :sonkv)
       :compression t))))
