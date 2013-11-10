(use :euler.util.my)


(print 
  (reverse
	(take 10 
	(reverse 
	  (getdigit 
		(loop for x from 1 upto 1000 sum (expt x x)))))))
