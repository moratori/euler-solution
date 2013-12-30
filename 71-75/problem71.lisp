
(use :euler.util.my)


(defun getrat% (d)
 (let1 res nil
	  (block exit
		(for (x (floor (* 3/7 d))) 
	   	     (> x (ceiling  (* 2/5 d))) 
	  	     (1- x) nil
			 (when (coprime? x d)
			   (setf res (/ x d))
			   (return-from exit)))) res))

;; 多分前回求めた値と 3/7 の間間って感じで
;; 範囲を絞っていけばもっと全然速くなる


(defun until-d (d)
  (sort 
	(time (label 
	  (main (acc result)
		(if (> acc d) result
		  (let1 factor (getrat% acc)
			(main (1+ acc)
				  (if (null factor) result (cons factor result)))))) 
	(main 2 nil))) (lambda (x y) (< x y))))



(defun main (end)
	(let1 r (until-d end)
	  (print 
		(numerator
		  (lastelm 
			(subseq r 0 (position 3/7 r)))))))



(print (main 1000000))
