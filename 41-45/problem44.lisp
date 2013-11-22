

(use :euler.util.my)


(defun pen? (n)
  (multiple-value-bind (u s)
	(floor (/ (1+ (sqrt (1+ (* 24 n)))) 6))
	(declare (ignore u))
	(zerop s)))

(defun pen (n) 
  (* n (1- (* 3 n)) 1/2))

(defun acc? (n)
  (div? (1- n) 3))



(defun searching (acc)
  (let1 pn (pen acc)
	(if (acc? pn)
	  (let1 subs  (- (pen (/ (1- pn) 3)) pn)
		 (if (pen? subs) subs
		   (searching (1+ acc)))) (searching (1+ acc)))))


#|

(setq tar (mapcar #'pen (range1-n 5000)))
(apply #'min [(abs (- x y)) | x <- tar y <- tar (and (pen? (+ x y)) (pen? (- (max x y) (min x y))))])
answer is 5482660

|#

(print (searching 2))



