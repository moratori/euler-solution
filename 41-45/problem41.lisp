

(use :euler.util.my)


(print 
  (apply #'max 
	(remove-if-not 
  		(lambda (x) (and (prime? x) (pandigital? x 7)))
  		(mapcar 
		  (lambda (x) (digit->num x))
  		  (perm '(1 2 3 4 5 6 7))))))














