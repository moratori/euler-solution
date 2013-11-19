

(use :euler.util.my)



(defun reverseadd (n)
  (+ n (digit->num (reverse (getdigit n)))))


(defun palindrome? (n)
  (let1 tmp (getdigit n)
	(equal tmp (reverse tmp))))

;;; 0 ~ 49 回の操作で
;;; いつかは回文になるかならないか
(defun lychrel? (n)
  (label 
	(main (acc n)
		(cond
		 ((zerop acc) t) 
		 ((palindrome? n) nil)
		 (t (main (1- acc) (reverseadd n))))) (main 48 (reverseadd n))))


(print 
  
(loop for x from 0 upto 10000 count (lychrel? x))
  )
