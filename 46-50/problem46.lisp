
(use :euler.util.my)


(defun suchodd? (a)
(label
	(main (n)
		  (if (>= (* 2 n n) a) nil
			(if (prime? (- a (* 2 n n))) t
		  		(main (1+ n)))))(main 1)))


(print (find-fn (complement #'suchodd?)  3 2 #'prime?))
