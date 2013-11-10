

(quicklisp:quickload :euler.util.my)
(use-package :euler.util.my)

(defun fib (n &optional (a 1) (b 1))
  (cond 
	((= n 1) a)
	((= n 2) b)
	(t 	(fib (1- n) b (+ a b)))))



(defun check (acc)
  (if (= 1000 (length (getdigit (fib acc))))
	acc
	(check (1+ acc))))


(print (check 1))
