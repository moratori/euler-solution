

(use :euler.util.my)

(defun check? (x)
  (destructuring-bind (a b c d e f g h i j k l m n o p q r s) (getdigit x)
	(declare (ignore b d f h j l n p r))
	(and (= a 1)
		 (= c 2)
		 (= e 3)
		 (= g 4)
		 (= i 5)
		 (= k 6)
		 (= m 7)
		 (= o 8)
		 (= q 9)
		 (= s 0))))

(print 
  (time (loop 
			named exit
			for x 
			from (+ 8 (floor (sqrt 1020304050607080900)))
			upto (floor (sqrt 1929394959697989990))
			by 10
			do (when (check? (* x x))
				 (return-from exit x)))))


