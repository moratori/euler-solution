

(ql:quickload :euler.util.my)
(import '(euler.util.my:erat
		  euler.util.my:let1
		  euler.util.my:euler-phi
		  euler.util.my:memoize
		  euler.util.my:set-equal?
		  euler.util.my:getdigit
		  euler.util.my:digit))

(use :iterate)



(erat 10000000)

;; こいつがダメだった
(defun such? (n)
  (let1 ep (euler-phi n)
	(and 
	 (set-equal? (getdigit n) (getdigit ep))
	 (= (digit n) (digit ep)))))


(defun such?% (n phi)
  (let ((nsp (getdigit n))
		(psp (getdigit phi)))
	(every 
	  (lambda (x)
		(= (count x nsp :test #'=)
		   (count x psp :test #'=)))
	  (remove-duplicates nsp :test #'=))))


;; 9999889 になるんだけど　まちがいっぽい
;; でけた

(print 
  (time (iter (for n from 2 to 9999999)
			  (let1 phi (euler-phi n)
				(when (such?% n phi)
		  			(finding n minimizing (/ n phi)))))))


