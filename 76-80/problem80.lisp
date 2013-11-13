
(use :euler.util.my)


;;; 1.4142135623730...
;;; sq(2) = a + b (a > 0 and 0 > b)
;;; a = 1
;;; 
;;; 1.4142135623730... -> 1
;;; 4.142135623730...  -> 4
;;; 1.42135623730...   -> 1
;;; 


;;; 有理数(分数表示で)が渡されることを意図している
(defun getsmall-num (n m)
  (labels 
	((main (n m result)
	  (if (zerop m) 
		(reverse result)
		(multiple-value-bind (a b) (floor n)
		  (main (* b 10) (1- m) (cons a result)))))) (main n m nil)))

;;; sqrt(n) の有理数
(defun getsqrt (n limit)
  (labels 
	((main (acc ax limit)
		(cond 
		  ((zerop limit) acc)
		  ((> (expt (+ acc ax) 2) n) 
		   (main acc (/ ax 10) (1- limit)))
		  (t (main (+ acc ax) ax limit))))) (main 0 1 limit)))

(defun target? (n) 
  (multiple-value-bind (a b) (floor (sqrt n))
	(declare (ignore a))
	(not (zerop b))))

(compile 'getsmall-num)
(compile 'getsqrt)

(print 
  (loop 
	for x 
	from 2 upto 100
	if (target? x)
	sum (sum (getsmall-num (getsqrt x 100) 100))))

