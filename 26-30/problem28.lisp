;(use :euler.util.my)


;; an = x^2 + (-x+1)n
(defun sumcorner (n)
  (if (= 1 n) 1
	(+ (* 4 n n) (* 6 (+ (* -1 n) 1)))))


(defun sum-nbyn (n &optional (result 1))
  (if (= n 1) result
	(sum-nbyn 
	  (- n 2) 
	  (+ result (sumcorner n)))))

(print (sum-nbyn 1001))
