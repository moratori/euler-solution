
(load "./mod.lisp")

(defun fib (n &optional (a 1) (b 2))
  (cond 
	((= n 1) a)
	((= n 2) b)
	(t (fib (1- n) b (+ a b)))))

(defun adder (n &optional (result 0) (fin 4000000))
  (let ((value (fib n)))
	(if (< value fin) 
	  (if (div? value 2) 
		(adder (1+ n) (+ result value) fin)
		(adder (1+ n) result fin)) result)))


(print (adder 1))
