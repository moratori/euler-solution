

(use :euler.util.my)


(declaim (inline join))

(defun join (a b)
  (declare (type number a b))
  (+ (* a (expt 10 (digit b))) b))


(defun such? (n)
  (declare (type number n))
  (let1 seed '(3 7 109 673)
		(and 
		  (not (member n seed :test #'=))
		  (every 
			(lambda (x)
			  (and 
				(prime? (join x n))
				(prime? (join n x)))) seed))))

;(print 
 ; (find-if #'such?
;		   (erat 200000000)))


;; => {3,7,109,673,129976621}
;; の集合はたしかに条件を満たすけど、最小でないっぽい
;; ４つの素数は問題のものだと仮定して
;; それで最小のものを探したら 129976621 だったんだけど
;; 違うわけだから、４つの素数も全部探さなきゃいけないってことだ
