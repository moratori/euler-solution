
(use :euler.util.my)


#|

	1/x + 1/y = 1/n <-> n(x+y) = xy
	x > 0 , y > 0 , n > 0
	xy > x + y 

	end condition:: xy/c > x+y
	

|#


(defun such? (n x y)
  (= (* n (+ x y)) (* x y)))


(defun search-solution% (n)
  (let1 solution nil
		(for (x (1+ n)) (<= x (+ (* n n) n)) (1+ x) nil
	   (for (y (1+ n)) (<= y (/ (* n x) (- x n))) (1+ y) nil
			(when (such? n x y)
			  (push (list x y) solution)
			  )))
		(length (remove-duplicates solution :test #'set-equal?))))


(defun search-solution (n)
  (let1 acc 0
		(block exit
		(for (x (1+ n)) (<= x (+ (* n n) n)) (1+ x) nil
		    (when (> x (/ (* n x) (- x n))) 
			  (return-from exit))
	   		(for (y (1+ n)) (<= y (/ (* n x) (- x n))) (1+ y) nil
				 (when (such? n x y)
				   (incf acc)
				   )))		   
			   )
	
		acc))



(print (loop named exit 
			 for n from 1260 by 1
			 do 
			 (when (> (search-solution n) 1000)
			   (return-from exit n))))

