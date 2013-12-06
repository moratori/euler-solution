
(use :euler.util.my)

(defun split-string (str ch)
  (labels
	((main (str acc  result)
	 	(cond 
		  ((string= str "")
		   (if (string= acc "") 
			 result 
			 (cons acc result)))
		  ((char= ch (char str 0))
		   (main (subseq str 1) 
				 "" 
				 (cons acc result)))
		  (t
			(main (subseq str 1)
				  (concatenate 'string acc (subseq str 0 1))
				  result)))))
	(reverse (main str "" nil))))

(defun read-data ()
  (with-open-file 
	(in "base_exp.txt"  :direction :input)
	[(list x
		   (mapcar #'parse-integer 
			 	(split-string (read-line in) #\,)))
	 	  | x <- (range1-n 1000 1)]))

(defun digit-exp (a n) 
  (floor (1+ (* n (log a 10)))))

(defun rough (data)
  (psort data
	(lambda (x y)
	  (destructuring-bind (ind-x (a n)) x
		(destructuring-bind (ind-y (b m)) y
		  (> (digit-exp a n)
			 (digit-exp b m)))))))

(let* ((all (rough (read-data)))
	   (fl  (apply #'digit-exp (second (car all)))))
  (let1 can (take (position-if 
	  (lambda (x)
		(/= fl (apply #'digit-exp (second x))))  all) all)
		(print 
		  (psort 
			can
			(lambda (x y)
			  (destructuring-bind (ix (a n)) x
				(destructuring-bind (iy (b m)) y
				  (> (expt a n) (expt b m)))))))))
