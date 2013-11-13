(use :euler.util.my)

(defun cut (n)
  (let*  ((tar (getdigit n))
		  (end (1- (length tar))))
	(list 
	  (range1-n end 0 (pa$ (flip #'nthcdr) tar))
	  (mapcar 
		#'reverse 
		(range1-n end 0 (pa$ (flip #'nthcdr) (reverse tar)))))))


(defun shorten-prime? (n)
  (destructuring-bind (lr rl) (cut n)
	(and 
	  (every #'prime? (mapcar #'digit->num lr))
	  (every #'prime? (mapcar #'digit->num rl)))))


(print 
  (labels 
	((main (start result)
		(if (= (length result) 11)
		  (sum result)
		  (main 
			(1+ start) 
			(if (shorten-prime? start) 
			  (cons start result)
			  result))))) (main 11 nil)))
