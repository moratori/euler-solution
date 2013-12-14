

(use :euler.util.my)


(defun count-ways% (n start &optional (result 0))
  (cond 
	((> 2 n) result)
	((> n start)
	 (count-ways% 
	   (- n start)  
	   start 
	   (1+ result)))
	((> start (- n start)) result)
	(t 
	  (count-ways% 
		(- n start)  
		start 
		(1+ result)))))


(defun count-ways (n)
  (loop for i from 1 upto
		(floor (/ n 2)) sum 
		(progn 
		  (let1 c (count-ways% n i)
			(format t "Start: ~A , Count ~A~%"i c)	
			c
			)
		  )))


(print (count-ways 6)) ;; -> 8
