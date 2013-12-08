
(use :euler.util.my)


(defun hazumi? (n)
  (let1 split (getdigit n)
	(not 
	  (or
		(apply #'<= split)
		(apply #'>= split)))))

(print 
  (block exit
	(let1 haz 0
	  (for (x 1) (< inf+) (1+ x) nil
	 		(when (hazumi? x) (incf haz))
			(when (>  (floor (* 100 (/ haz x))) 98)
			  (return-from exit x))))))
