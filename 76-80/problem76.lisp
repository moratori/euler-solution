

(defun express (n use)
	(cond 
		((or (< n 0) (< use 1))
		 0)
		((zerop n) 1)
		((= use 1) 1)
		(t (+ (express (- n use) use)
					(express n (1- use))))))

(defun ways (n)
	(loop for x from 1 below n
				sum (express x (- n x))))


(print 
	(time (ways 100)))
