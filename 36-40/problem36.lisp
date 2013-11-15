

(use :euler.util.my)


(defun palindrome? (n)
  (let ((a (getdigit n))
		(b (format nil "~B" n)))
	(and 
	  (= n (digit->num (reverse a)))
	  (string= b (reverse b)))))


(print 
 (time (loop for x from 0 upto 999999 if (palindrome? x) sum x))) 
