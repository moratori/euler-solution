
(use :euler.util.my)


(defun shifts (n)
  (if (> 10 n) (list n)
	(labels 
	  ((main (n result lim)
		(if (zerop lim) (reverse result )
		  (main  
			(let1 tmp (getdigit n)
			(digit->num 
				(append1 (cdr tmp) (car tmp))))
			(cons n result) 
			(1- lim)))))
	  	(main n nil (length (getdigit n))))))


(defun circular-prime? (n)
  (if (member 0 (getdigit n)) nil
	(every #'prime? (shifts n))))


(print (loop for x in (erat 1000000) 
			 count (circular-prime? x)))
