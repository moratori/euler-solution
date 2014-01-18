
(use :euler.util.my)

#|

28 = 2^2 + 2^3 + 2^4
33 = 3^2 + 2^3 + 2^4
49 = 5^2 + 2^3 + 2^4
47 = 2^2 + 3^3 + 2^4

a  = p^2 + p^3 + p^4 

|#


(erat (+ 500 (floor (sqrt 50000000))))

;;; n を構成する素数piはどう考えてもsqrt(n)以下
(defun endval (n) 
  (floor (sqrt n)))

;; これでnを表すのに必要な素数のリストを得る
(defun for-n-prime (n)
  (erat (endval n)))


;;; n < a^m
(defun exceed? (a p n)
  (<= a (expt p n)))


;; 4乗の数を選ぶ時にさ、その4乗してみて最低 12余さないとダメだ
;; じゃないと 3乗と2乗で0にできるわけない!!
;; 同じ理由で3乗選ぶ時も同じようにできるかな
(defun cand (a n)
  (let1 tar (for-n-prime a)
	(remove-if 
		(lambda (x)
		  (exceed? a x n))  tar)))



(defun cand4 (n)
  ;; 本当は 4乗根求めてeratに掛けるのがいいんだけど
  ;; nth-rootが遅いのでダメだ
  ;; しょうがないのでsqrt
  (let1 tar (erat (floor (sqrt n))) 
	(remove-if 
	  (lambda (x)
		(let1 num (expt x 4)
			(or (<= n num)
				(> 12 (- n num)))))tar)))

(defun cand3 (n)
  ;; 本当は 3乗根求めてeratに掛けるのがいいんだけど
  ;; nth-rootが遅いのでダメだ
  ;; しょうがないのでsqrt
  (let1 tar (erat (floor (sqrt n))) 
	(remove-if 
	  (lambda (x)
		(let1 num (expt x 3)
			(or (<= n num)
				(> 4 (- n num)))))tar)))



(defun such?% (n)
  (let1 lst (list (cand n 4)
				  (cand n 3)
				  (cand n 2))
		(if (some #'null lst) nil
		  (some 
			(lambda (fth)
			  (some 
				(lambda (trd)
				  (some 
					(lambda (snd)
					  (zerop (- n (expt fth 4)
								  (expt trd 3)
								  (expt snd 2))))
					(remove-if 
					  (lambda (x)
						(> (expt x 2)
						   (- n (expt fth 4) (expt trd 3))))
					  (third lst))))
				(remove-if 
				  (lambda (x)
					(> (expt x 3) 
					   (- n (expt fth 4)))) 
				  (second lst)))) 
			(first lst)))))



;; 4乗と三乗を引いたやつが二乗数になりかつ素数であるかで判定
(let ((square?-memo (memoize #'square?))
	  (prime?-memo  (memoize #'prime?)))
 (defun such? (n)
  (let1 lst (list (cand4 n)
				  (cand3 n))
		(if (some #'null lst) nil
		  (some 
			(lambda (fth)
			  (some 
				(lambda (trd)
				  (let1 num (- n (expt fth 4) (expt trd 3))
					(and
					    (> num 0)
					  	(funcall square?-memo num) 
						(funcall prime?-memo (floor (sqrt num))))))
				(second lst))) 
			(first lst))))))




;; 10000 => 8.789 :: such?% 
;; 10000 => 4.342 :: such?
(print (time (loop for x from 28 below 50000000 count (such? x))))
;;

;;(print (time (such? 5000000)))
;;(print (cand 707285 4))


