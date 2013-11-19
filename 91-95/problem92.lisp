


(use :euler.util.my)

;; brute forced

(defun check (n)
  (cond 
	((= n 1) nil)
	((= n 89) t)
	(t (check 
		 (sum 
		   (mapcar 
			 (lambda (x) (* x x)) (getdigit n)))))))

(compile 'check)


(time 
  (print (loop for x from 1 upto 9999999 count (check x))))
