
(use :euler.util.my)


(defun call (seed func arg)
  (if (null arg) seed
	(call (funcall func (car arg) seed ) func (cdr arg))))

;; 4431 4441
;; set-difference
(defun replaceble (x y)
  (let ((xl (getdigit x))
		(yl (getdigit y)))
		(and (set-equal? xl yl) (null (call yl #'remove-one xl)))))


(defun relation? (l)
  (label
	(main (l)
		(cond 
		  ((= (length l) 3)
		   (destructuring-bind (a b c) l
			 (if (= (- c b) (- b a)) (list a b c) nil)))
		  (t 
			(destructuring-bind (a b c . e) l
			  (if (= (- c b) (- b a)) (list a b c) (main (cdr l)))))))
	(main (psort l))))

(let* ((src [x | x <- (range1-n 9999 1000) (prime? x)]))
	  (let1 result nil
		(dolist (x src)
		  (let1 tmp nil
			(dolist (y src)
			  (when (and  (replaceble x y))
				(push y tmp)))
			(when (and (not (null tmp)) 
					   (< 2 (length tmp))) 
			  	(push tmp result)
			  )))
		(print (remove-if #'null (mapcar #'relation? (remove-duplicates result :test #'set-equal?))))))	


