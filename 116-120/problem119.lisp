

(use :euler.util.my)


(defun such? (n)
  (if (> 10 n) nil
	(let1 dsum (sum (getdigit n))
		(cond 
		  ((= dsum 1) nil)
		  ((not (div? n dsum)) nil)
		  (t (= (expt dsum  (floor (log n dsum))) n ))))))


(defun findn (n)
  (label
	(main (acc start result)
		  (if (zerop acc) result
			(let1 each (find-fn #'such? start)
				(main (1- acc) (1+ each) (cons each result))
			)
			)
		)
	(main n 10 nil))
  )


;; (1679616 614656 390625 234256 19683 17576 5832 4913 2401 512 81)
;; 2.23
;; 26.128
;;(print (time (findn 12)))
;; 25.829
(print (time (findn 12)))

