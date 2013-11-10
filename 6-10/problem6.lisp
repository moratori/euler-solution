

(defun sqsum (n)
  (* 1/6 n (+ (* 2 n) 1) (+ n 1)))

(defun sumsq (n)
  (expt (* 1/2 n (+ n 1)) 2))


(print (- (sumsq 100) (sqsum 100)))
