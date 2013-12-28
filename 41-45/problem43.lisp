
(use :euler.util.my)

;;;; --dynamic-space-size : 2000


;;; satisfies purely condition
(defun such? (n)
  (and (pandigital? n :start 0)
	   (destructuring-bind 
		 (d1 d2 d3 d4 d5 d6 d7 d8 d9 d10) (getdigit n)
		 (declare (ignore d1))
		 (and 
		   (div? (digit->num (list d2 d3 d4))  2)
		   (div? (digit->num (list d3 d4 d5))  3)
		   (div? (digit->num (list d4 d5 d6))  5)
		   (div? (digit->num (list d5 d6 d7))  7)
		   (div? (digit->num (list d6 d7 d8))  11)
		   (div? (digit->num (list d7 d8 d9))  13)
		   (div? (digit->num (list d8 d9 d10)) 17)))))

;;; '(0 1 2 3 4 5 6 7 8 9)
;;;  10 9 8 7 6 5 4 3 2 1
;;; 9! = 362880
;;;
;;; (1 2 3)
;;; 1 2 3
;;; 1 3 2
;;; 2 1 3
;;; 2 3 1
;;; 3 1 2
;;; 3 2 1

(print 
  (loop for x in (subseq (perm '(0 1 2 3 4 5 6 7 8 9)) (fact 9))
	  for each = (digit->num x)
	  if (such? each)
	  sum each)
  )





