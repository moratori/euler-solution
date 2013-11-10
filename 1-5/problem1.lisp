
(load "mod.lisp")



(print 
  (sum 
	(filter 
	  (lambda (x) (or (div? x 3) (div? x 5))) (range1-n 999))))
