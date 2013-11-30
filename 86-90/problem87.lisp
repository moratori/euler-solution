
(use :euler.util.my)

#|

28 = 2^2 + 2^3 + 2^4
33 = 3^2 + 2^3 + 2^4
49 = 5^2 + 2^3 + 2^4
47 = 2^2 + 3^3 + 2^4

a  = p^2 + p^3 + p^4 

|#


;;; n を構成する素数piはどう考えてもsqrt(n)以下
(defun endval (n) 
  (floor (sqrt n)))

;; これでnを表すのに必要な素数のリストを得る
(defun for-n-prime (n)
  (erat (endval n)))


;;; n < a^m
(defun exceed? (a p n)
  (< a (expt p n)))



(defun cand (a n)
  (let1 tar (for-n-prime a)
	(remove-if 
		(lambda (x)
		  (exceed? a x n))  tar)))


;;; a
;;; ((2 3 5 7 11) (2 3 5 7) (2 3 5))
;;; 3 13
(defun check (n limit)
  (let1 can (cand n limit)
	(cond
	  ((< limit 2) nil)
	  ((null can) nil)
	  ((= limit 2) 
	   (and (square? n) (member (endval n) can)))
	  ((= (length can) 1)
	   (check (- n (expt (car can) limit)) (1- limit))
	   )
	  (t
		(some 
		  (lambda (x)
			(check (- n (expt x limit)) (1- limit))
			)
		  can)
		)
	  )	
	)
  )


;;; 遅すぎておわらん
;;; 要改良
(print (loop for x from 0 upto 49 count (check x 4)))



