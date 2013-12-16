

(ql:quickload :euler.util.my)
(import '(euler.util.my:enumdiv))
(use :iterate)


(defun tds (n)
  (- (apply #'+ (enumdiv n)) n))


(defun chain (n &optional result)
  (print n)
  (sleep 1)
  (cond
	((> n 1000000) nil)
	((zerop n) nil)
	((member n result) (reverse result))
	(t (chain (tds n) (cons n result)))))

#|

(print 
  (iter (for i from 1 to 1000000) 
		(print i)
	  (finding i maximizing (length (chain i)))))
|#

(print (chain 402170))
