
(use :euler.util.my)


(defun getrat (d)
  (loop for n 
		from  (floor (1+ (* 1/3 d)))
		below (* 1/2 d)
		count 
		(coprime? n d)))


(defun until-d (n)
  (loop for d 
		from 2
		upto n
		sum (getrat d)))


(print (until-d 12000))
