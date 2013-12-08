
(use :euler.util.my)


;; satisfies purely condition
(defun such? (n)
  (and (pandigital? n :start 0)
	   (destructuring-bind 
		 (d1 d2 d3 d4 d5 d6 d7 d8 d9 d10) (getdigit n)
		 (declare (ignore d1))
		 (and 
		   (div? (digit->num (list d2 d3 d4)) 2)
		   (div? (digit->num (list d3 d4 d5)) 3)
		   (div? (digit->num (list d4 d5 d6)) 5)
		   (div? (digit->num (list d5 d6 d7)) 7)
		   (div? (digit->num (list d6 d7 d8)) 11)
		   (div? (digit->num (list d7 d8 d9)) 13)
		   (div? (digit->num (list d8 d9 d10)) 17)))))

;; 条件を満たす 3桁の数を返す
(defun sat-num (prime)
  (remove-if-not 
	(lambda (x) (and (div? x prime) (duplicate-not? (getdigit x))))
	(range1-n 999 100)))


;; l1で使える数
(defun useble (l1 l2)
  (remove-if-not 
  	(lambda (x)
		(member-if 
	  		(lambda (y)
				(equal (cdr (getdigit x)) (cdr (getdigit y)))) 
			l2)) l1))


(print (sat-num 2))
(print (sat-num 3))
