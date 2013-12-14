
(use :euler.util.my)



#|

					     1
	sqrt(N) = a0  +   -------
					  a1 +    1
						   -------
						   a2 +    1
							    -------
								a3 +    1
									 -------
									  a4  ...

	

|#

;; n is root!!
(defun foo (n)
  (let1 a (getsqrt n 1)
		(values a (/ (* 2 a)  (- n (* a a))))))



(foo 2)
