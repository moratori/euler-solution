

(use :euler.util.my)


;;; fib(x) > 10^9 -> x > 38
;;; 39 から見てく

(defun such? (n)
  (let1 split (getdigit n)
	(and 
	  (pandigital? (digit->num (take 9 split)))
	  (pandigital? (digit->num (take 9 (reverse split))))
	  )))



(print 
  
(find-fn (lambda (x) (such? (fibiter x))) 1)
  )



