(use :euler.util.my)


(let1 domain (cdr (range1-n 100))
(print 
  (length 
	(remove-duplicates 
		[(expt x y) | x <- domain y <- domain]))))
