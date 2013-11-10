
(quicklisp:quickload :euler.util.my)
(use-package :euler.util.my)

#|
(defun lst->num (lst &optional (result 0))
  (if (null lst) result
	(lst->num 
	  (cdr lst) 
	  (+ result(* (car lst) (expt 10 (1- (length lst))))))))


(defun complstnum< (lst1 lst2)
  (< (lst->num lst1) (lst->num lst2)))


(dolist (x 
		  
(psort (perm '(1 2 3 4))  #'complstnum<)
		  )(print x))
|#




(defun nth-sorted-perm (n lst &optional (result nil))
  (if (null lst) result
	(let1 target (psort lst)
	 (if (zerop n)  (reverse (append (reverse target) result))
	   (let* ((index (fact (1- (length lst))))
			  (head (nth (div n index) target)))
		 (nth-sorted-perm 
		   (mod n index) 
		   (remove-one head target) 
		   (cons head result)))))))

(compile 'nth-sorted-perm)

(time (nth-sorted-perm 999999 '(0 1 2 3 4 5 6 7 8 9)))



