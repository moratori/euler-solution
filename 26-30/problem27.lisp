
(use :euler.util.my)



(defun make (a b)
  (lambda (n) 
	(+ (* n n) (* a n) b)))

;;; 連続何回素数を吐くか
(defun leneq (f)
  (label 
	(main (acc)
	 (if (prime? (funcall f acc)) 
	   (main (1+ acc))
	   acc))
	(main 0)))



;; n^2 + an + b > 1
;; b > 1 - an - n^2
;; 
;; 任意のnについて b > (1 - n(n+a)) が成り立つような b を取らなければいけない
;; -999<= b <= 999
;; n >= 0
;; n == 0 のとき
;; b > 1
;; 少なくとも2以上



(let ((result 1) (len 0))
	  (for (a -999) (< a 1000) (1+ a) result
		(for (b 2) (< b 1000) (1+ b) nil
			 (let1 plen (leneq (make a b))
				 (when (> plen len)
			  		 (setf result (* a b) 
						   len plen)))))
	  (print result))



