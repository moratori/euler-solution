
(use :euler.util.my)


(defun such? (x a)
  (= a (length (remove-duplicates (factr x)))))

(print 
  (find-fn 
	(lambda (x)
	  (and 
		(such? x 4)
		(such? (1+ x) 4)
		(such? (+ x 2) 4)
		(such? (+ x 3) 4)
		)
	  ) 2)
  )

