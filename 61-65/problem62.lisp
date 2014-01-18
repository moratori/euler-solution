
(use :euler.util.my)

#|

	まずそのような数は3桁以上の数である
	なぜなら各桁の並び替えが 5つ以上の種類に成るのは３桁体から
	

|#


(defun check? (n)
  (and (cube? n)
	   (= 5 (count-if 
			  (lambda (x) 
				(when 	(cube? (digit->num x))
				  (print x)
				  t)
				)
			  (remove-if 
				(lambda (x) (< (digit->num x)  n)) (remove-duplicates (perm (getdigit n)) :test #'equal)))) ))


(defun seek (acc)
  (if (check? acc) acc
	(seek (1+ acc))))


(print (time (seek 100)))

