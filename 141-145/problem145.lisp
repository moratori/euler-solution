
(use :euler.util.my)


(defun reversible? (n)
  (let1 dn  (getdigit n)
		(if (or (zerop (lastelm dn))
				(zerop (car dn)))
		  	nil
			(every #'oddp
		 		(getdigit 
		   		(+ n (digit->num (reverse dn))))))))

(defun great (num num-lst)
  (length 
	(remove-if-not 
	  (lambda (x)
		(> (digit->num (reverse (getdigit x)))
		   num))
	  num-lst)))

(defun make (n)
  (loop for x from 10 
		below n
		if (reversible? x)
		collect x))



(print (loop for x from 10 below 1000000000 count (reversible? x)))
