

(use :euler.util.my)

(defun such? (n)
 
	(and 
	  (pandigital? (bottom-digit n 9))
	  (pandigital? (top-digit n 9))
	  ))


(block exit
	(for (x 40) (< x INF+) (1+ x) nil
		 (when (such? (fibmat x))
		   (print x)
		   (return-from exit nil))))




