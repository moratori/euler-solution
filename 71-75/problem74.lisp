
(use :euler.util.my)

(defun digit-fact (n)
  (sum (mapcar #'fact (getdigit n))))


;;; 3よち小さいかずと40585の存在

(defun loop? (n)
  (if (< n 3) 0
	(case n
	(145 0)
	(40585 0)
	(169 2)
	(363601 2)
	(1454 2)
	(871 1)
	(45361 1)
	(872 1)
	(45362 1)
	(t nil))
	
	)
  
  )


(defun len (n &optional (acc 1))
  ;(print n)
  (let1 check (loop? n)
	(if check (+  acc check)
	  (len (digit-fact n) (1+ acc))
	  )	
	)
  )


(print 
 (count-if 
	(lambda (x) 
	  (= 60 (len x))
	  ) 
	(range1-n 999999)))

