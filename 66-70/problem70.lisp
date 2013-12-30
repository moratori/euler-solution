

(ql:quickload :euler.util.my)
(import '(euler.util.my:erat
		  euler.util.my:let1
		  euler.util.my:euler-phi
		  euler.util.my:memoize
		  euler.util.my:set-equal?
		  euler.util.my:getdigit
		  euler.util.my:digit))

(use :iterate)


(setf 
  (symbol-function 'euler-phi)
  (memoize #'euler-phi))

(erat 10000000)

(defun such? (n)
  (let1 ep (euler-phi n)
	(and 
	 (set-equal? (getdigit n) (getdigit ep))
	 (= (digit n) (digit ep)))))


(print 
  (iter (for each in 
		   (iter (for x from 2 to 9999999) 
				 (when (such? x) (collect x))))
	  (finding each minimizing (/ each (euler-phi each)))))





