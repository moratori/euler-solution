
(quicklisp:quickload :euler.util.my)
(use-package :euler.util.my)


(defun compnum? (n)
  (let1 total (- (sum (enumdiv n)) n)
  	(cond 
	  ((= n total) 0)
	  ((> total n) 1)
	  ((< total n) -1))))


(defconstant +abundant+ (filter x (= (compnum? x) 1) (range1-n 28123)))

(defun express-plus? (n)
  (if (< n 24) nil
	(labels 
	  ((main (n data)
		(cond 
		  ((null data) nil)
		  ((< (- n (car data)) 12) nil)
		  ((member (- n (car data)) data) t)
		  (t (main n (cdr data)))))) (main n +abundant+))))


(print (loop for x from 1 upto 28123 if (not (express-plus? x)) sum x))
