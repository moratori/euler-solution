
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

(defun check (a b c d e f g)
  (let ((sa (getdigit a))
		)
	t)
  )


(setq 
  c3 
  (intersection 
	(useble (sat-num 3) (sat-num 2)) 
	(useble (sat-num 3) (sat-num 5)))
  c2
  (useble (sat-num 2) c3)
  c5
  (intersection 
	(useble (sat-num 5) c3)
	(useble (sat-num 5) (sat-num 7)))
  c7
  (intersection
	(useble (sat-num 7) c5)
	(useble (sat-num 7) (sat-num 11)))
  c11
  (intersection
	(useble (sat-num 11) c7)
	(useble (sat-num 11) (sat-num 13)))
  c13
  (intersection
	(useble (sat-num 13) c11)
	(useble (sat-num 13) (sat-num 17)))
  c17
  	(useble (sat-num 17) c13))

#|
[(list a b c d e f g) 
 	| a <- (sat-num 2)
	  b <- (sat-num 3)
	  c <- (sat-num 5)
	  d <- (sat-num 7)
	  e <- (sat-num 11)
	  f <- (sat-num 13)
	  g <- (sat-num 17) (check a b c d e f g)]
|#


(print c5)
