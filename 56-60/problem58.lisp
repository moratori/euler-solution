

(use :euler.util.my)


(defun nbyn-diag (n)
  (label 
	(main (n result)
		(if (= n 1) result
			(let1 c (1+ (expt (- n 2) 2))
				(main
	  				(- n 2) 
	  				(append 
						(loop 
	 		 				for i from (- n 2) upto (- (* 3 n) 4) 
	  		 				by (1- n)
							collect (+ c i)) (list (* n n)) result))))) 
	(main n '(1))))


(defun percentage< (n p)
  (let1 tar (nbyn-diag n)
		(< (print(* 100 (/ (count-if (lambda (x) (prime? x)) tar)  (length tar)))) p)
	))


;; prime?? 5787 0.03

(defun main (n)
  (if (percentage< n 10) n
	(main (+ n 2))))

(compile 'prime??)
(compile 'nbyn-diag)
(compile 'percentage<)
(compile 'main)


;;; 26247 is True
;;;	26245 also True
;;; 26243 also True
;;; 26241 also True
;;; 26239 is NOT True


;;; まず高速なprime??でだいたい探索して
;;; prime?で正確に出す

;(format t "THE ANSWER = ~A~%" (main 5787)) ;; -> 26247
(print (main 26001))




