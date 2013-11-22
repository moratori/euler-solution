
(use :euler.util.my)

#|

1: 0


 	   1
2:	-------
     1 + 0


       1
3:	-------
	 1 +    1
		 -------
		  2 + 0

	   1
4:  -------
     1 +    1
		 -------
		  2 +    1
			  -------
			   1 + 0

	   1
5:  -------
     1 +    1
		 -------
		  2 +    1
			  -------
			   1 +    1
				   -------
					1 + 0

	   1
6:  -------
     1 +    1
		 -------
		  2 +    1
			  -------
			   1 +    1
				   -------
					1 +    1
					    -------
						 4 + 0


 1  2  3  4  5  6  7  8  9  10 11  12  13  14  15  ...
 1  2  1  1  4  1  1  6  1  1  8   1   1   10  1   ... 

 2 , 5 , 8 , 11 , 14  , 17 , 20 ... 3n - 1
 2 , 4 , 6 ,  8 , 10  , 12 , 14 ... 2n

2 3 4 5 6 7 8 9 10 ... 
1 2 1 1 4 1 1 6 1  ...

if div? i 3 : i - (i/3)

|#

(defun num (index)
  (if (div? index 3)
	(- index (/ index 3)) 1))


(defun approx-e% (n)
  (+ 2 (label 
	(main (acc)
		  (if (= acc n) 0
			(/ 1 (+ (num acc) (main (1+ acc))))))
	(main 2))))

(defun approx-e (n) 
  (approx-e% (1+ n)))



(print 
  
(sum (getdigit (numerator (approx-e 100))))
  )

