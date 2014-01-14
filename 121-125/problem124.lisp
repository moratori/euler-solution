
(use :euler.util.my)


(erat 100001)


(defun rad (n)
  (if (= n 1) 1
	(reduce 
	(lambda (x y) 
	  (* x (car y)))
	(factr-group n)
	:initial-value 1)))


(defun make (n)
  (sort 
	(loop for x from 1 upto n
		  collect (list x (rad x)))
	(lambda (a b) (< (second a) (second b)))))


(print (car (nth (1- 10000) (make 100000))))
