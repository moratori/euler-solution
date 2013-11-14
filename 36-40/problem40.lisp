
(use :euler.util.my)

;;  1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 ...
;;  1 2 3 4 5 6 7 8 9  1  0  1  1  1  2  1  3  1  4  1  5  1  6  1  7  1  8 ...
;;0 1 2 3 4 5 6 7 8 9  1  0  1  1  1  2  1  3  1  4  1


;; 10 11 12 13 ... 99

;; 1 * (10 - 1)     = 9
;; 2 * (100 - 10)   = 180
;; 3 * (1000 - 100) = 2700


;;; 全ての n 桁の数の 桁の和
(defun digit-sum (n)
  (let1 a (expt 10 n)
	(* n (- a (/ a 10)))))

;;; n までの 同じ桁の和
;;; n = 123 だったら
;;; 3(100) + 3(101) + 3(102) + ... 3(122)
(defun digit-sum-mod (n)
  (let1 d (length (getdigit n))
	(* d (- n (expt 10 (1- d))))))

;;; n はインデクスが何からはじまるか
(defun start-with (n)
   (+  1
	(loop
	  for i 
	  from 1 upto (1- (length (getdigit n))) 
	  sum (digit-sum i))
	(digit-sum-mod n)))

(defun solution (targetlst)
  (label
	(main (acc tar res)
	  (if (null tar) res
		(let* ((start (start-with acc))
			   (each (getdigit acc))
			   (end   (1- (+ start (length each)))))
		  (if (<= start (car tar) end)
			(main (1+ acc) (cdr tar) (* res (nth (- (car tar) start) each)))
			(main (1+ acc) tar res)))))(main 1 targetlst 1)))

(print 
  (solution '(1 10 100 1000 10000 100000 1000000)))
