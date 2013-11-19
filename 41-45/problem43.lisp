
(use :euler.util.my)



(defconstant +P+ (erat 17))
;;; '(2 3 5 7 11 13 17)

(defun check (n)
  (let1 a (getdigit n)
	(and 
	  (null (set-difference '(0 1 2 3 4 5 6 7 8 9) a)) 
	  (= 7 (loop 
		   for i from 2 upto 8
		   count (div? 
				   (digit->num 
					 (list 
					   (nth (1- i) a)
					   (nth i a)
					   (nth (1+ i) a)))
				   (nth (- i 2) +P+)))))))


#|

d1 d2 d3 d4 d5 d6 d7 d8 d9 d10

d2 d3 d4   :: d4 is even
d3 d4 d5   :: d3 + d4 + d5 is dividable 3 
d4 d5 d6   :: d6 is 5 or 0
d5 d6 d7   :: 
d6 d7 d8   :: 
d7 d8 d9   ::
d8 d9 d10  ::

|#

;; 1 2 3 5  9 8 7 7 5 2 -> 1 2 3 6 000000
;; 1 2 3 6  0 0 0 0 0 0 - 1 2 3 5 9 8 7 7 5 2
;;
;; 1 2 3 9  4 6 5 3 9 1 -> 1 2 4 0 000000
;; 1 2 4 0  0 0 0 0 0 0 - 1 2 3 9 4 6 5 3 9 1

;;; 1234567890
(defun main (start end &optional (result 0))
  (if (> start end) result
	(let1 s (getdigit start)
		  (cond 
			((oddp (nth 3 s))
			 (main 
			   (+ start 
				  (- (digit->num 
					   (append 
						 (take 3 s) 
						 (list (mod (1+ (nth 3 s)) 10)) 
						 (make-list 6 :initial-element 0))))) end result))

			)
	 )
	)
  )


(compile 'check)
(compile 'main)


(print (main 1023456789 9876543210))
