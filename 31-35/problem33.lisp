

(use :euler.util.my)


(defun redaction=? (origin up down)
  (if (zerop down) nil (= origin (/ up down))))



(print 
  (prod 
	(mapcar 
	  (lambda (x) 
		(/ (first x) (second x)))
	  (remove-if-not 
		(lambda (tup)
	  		(destructuring-bind (u d) tup
				(let* ((a (getdigit u)) 
			   		   (b (getdigit d)) 
			   		   (inter (intersection a b))) 			
		  			(if (null inter) nil
			  			(redaction=? 
							(/ u d)
							(car (remove-one (car inter) a)) 
							(car (remove-one (car inter) b)))))))
		[(list a b) | 
				a <- (range1-n 99 10) 
			    b <- (range1-n 99 10) 
			    (and (> b a) (not (and (div? a 10) (div? b 10))))]))))


