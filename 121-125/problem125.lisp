

(use :euler.util.my)



(defun palindrome? (n)
  (= n (digit->num (reverse (getdigit n)))))


(defun subs (n start)
  (cond
	((zerop n) t)
	((> 0 start) nil)
	((> 0 n) nil)
	(t 	(subs (- n (* start start)) (1- start)))))

;; 連続する平方数の和で表せるか?
(defun such? (n)
  (if (square? n) nil
	 (label
	(main (start)
		(cond
		  ((> 2 start) nil)
		  ((subs n  start) 
		   t)
		  (t (main (1- start)))))
	(main (ceiling (1- (sqrt n)))))))


(print (loop for x 
			 from 1 
			 below (expt 10 8)
			 if (and (palindrome? x) (such? x))
			 sum x))

