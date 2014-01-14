
(use :euler.util.my)


(defun r (a n)
  (mod (+ (expt (1- a) n) 
		  (expt (1+ a) n)) (* a a)))


(defun collect-cycle (a)
  (let1 head (r a 1)
	(cons head 
		  (loop for n from 2
		  	for each = (r a n)
		  	while (/= head each)
		  	collect each))))

(defun cycle-max (a)
  (apply #'max (collect-cycle a)))


(print (sum (mapcar #'cycle-max (range1-n 1000 3))))
