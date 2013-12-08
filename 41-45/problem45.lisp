
(use :euler.util.my)

(defvar *cache* 165)

(defun solve-2eq (a b c) 
  (/ (+ (* -1 b) (sqrt (- (* b b) (* 4 a c))))  
	 (* 2 a)))


(defun pen? (n)
  (label
	(main (acc)
	(let1 num (/ (* acc (1- (* 3 acc)))  2)
		  (cond
			((> num n) nil)
			((= num n)  t)
			(t (main (1+ acc)))))) 
	(let1 f (main *cache*)
		  	(when (not f)
	  (setq *cache* (floor (solve-2eq 3 -1 (* -2 n))))) f)))


(defun tri-hex (n)
  (+ n (* 2 n (1- n))))

(print 
  (time 
	(tri-hex 
	(find-fn
	  (lambda (x) (pen? (tri-hex x))) 144))))


