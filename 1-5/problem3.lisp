
(load (compile-file "mod.lisp"))

;;; 失敗したバージョン
;;; 100000000000回くらいまでループしなきゃいけないから死ぬ
;;; しかも素数には使えない
(defun largest-factor% (n)
  (let1 fin (/ n 2)
		(labels 
		  ((main (acc result)
				 (if (> acc fin) result
				   (main (+ acc 2) 
						 (if (and (div? n acc) (prime? acc)) 
						   (cons acc result) 
						   result) )))) (main 3 nil))))

;;; 一番小さい素因数を返す
(defun least-prime (n &optional (acc 3))
  (cond
	((< n 3) 1)
	((and (div? n acc) (prime? acc)) acc) 
	(t (least-prime n (+ acc 2)))))


;;; n = p1 * p2 * ... * pm
;;; n/(p1) = p2 * ... * pm
;;; n(p1*p2) = p3 * ... * pm
;;; .
;;; .
;;; .
;;; n/(p1*p2*...*pm-1) = pm をつかってる
(defun largest-factor (n)
  (let1 mini (least-prime n)
		(if (= n mini) mini 
		  (largest-factor (/ n mini)))))


(print (largest-factor 600851475143))
