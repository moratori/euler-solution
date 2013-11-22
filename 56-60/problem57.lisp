
(use :euler.util.my)

(defun root2 (n)
  (1+ (label 
	(main (acc result)
		(if (zerop (1- acc)) result
		  (main (1- acc) (/ 1 (+ result 2)))))(main n 1/2))))

(defun len (n)
  (length (getdigit n)))


(print 
  (count-if 
	(lambda (x)
	  (let ((u (numerator x))
			(d (denominator x)))
		(> (len u) (len d)))
	  ) (range1-n 1000 1 #'root2)))
