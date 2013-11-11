

(use :euler.util.my)


(defun pandigital? (n)
  (set-equal? (getdigit n) '(1 2 3 4 5 6 7 8 9)))


(defun getcan (n)
  (labels 
	((main (acc result)
	   (if (> (length result)  8)
		 result
		 (main (1+ acc) (append result (getdigit (* n acc))))))) (main 1 nil)))


(print 
  (apply #'max
	(remove-if-not #'pandigital?
(mapcar #'digit->num
		(remove-if-not 
  			(lambda (x) (= (length x) 9))
  				(print (mapcar #'getcan (range1-n 9999)))))
	))) 




