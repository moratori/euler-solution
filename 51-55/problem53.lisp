
(use :euler.util.my)

#|
(print 
  (sum 
	(loop for n from 0 
	  upto 100 
	  collect 
	  (loop for r from 0 
			upto n 
			count (> (select n r) 1000000)))))
|#

(print 
  (length [(list x y) | x <- (range1-n 100) y <- (range1-n x) (> (select x y) 1000000)]))
