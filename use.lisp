



;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames ".quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;;; 
(defun use (&rest syms)
  (let ((*standard-output* 
		  (make-string-output-stream)))
	(dolist (sym syms)
	  (ql:quickload sym)
	  (use-package sym))))
