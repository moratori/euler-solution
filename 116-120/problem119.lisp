

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
				(main (1- acc) (1+ each) (cons each result)))))
	(main n 10 nil)))


#|
	今の実装で a13 まで見つけるのに
	51.806sec
	(34012224 17210368 1679616 614656 390625 234256 19683 17576 5832 4913 2401 51281)
 
|#














;; a を 何乗すれば n桁を超えるか
(defun exceed (a n)
  (floor (1+ (log (expt 10 (1- n))  a))))


(defun domain (a n)
  ;; n桁以上の数で n+1桁未満の数を集める
  ;; aを底として
  ;; a > 1
  (loop for ex from (exceed a n)
		upto   (1- (exceed a (1+ n))) 
		collect (expt a ex)))


;; n桁の数でそういう奴を探す
(defun target (n)
  (label 
	(main (acc result)
	 (if (= acc (* 9 n)) result
	   (main (1+ acc)
			 (cons 
			   (remove-if-not 
				 (lambda (x)
				   (= (sum (getdigit x)) acc))
				 (domain acc n))
			   result))))
	
	(mapcan (lambda (x)x) (main 2 nil))))




#|
	今の実装で a13 まで見つけるのに
	51.806sec
	(34012224 17210368 1679616 614656 390625 234256 19683 17576 5832 4913 2401 51281)
 
|#


#|
	
	ある数 n = a1a2a3a4... について
	桁の和は 1 <= dsum(n) <= 9 * d(n)
	までになる! (dsumは各桁の和を返す関数 , dは桁数を返す関数)
	だからその間の数を底にして d(n) 桁に成るように
	累乗して、そいつらの中から和がnに成るやつを探せばいいんだ!

|#



;; n こ以上みつかるまで繰り返す
(defun main (n)
  (label 
	(main (cnt keta result)
		  (let* ((each (target keta))
			   	 (len (length each)))
			(if (> (+ cnt len) n) 
			  (append result each)
			  (main 
				(+ cnt len) 
				(1+ keta) 
				(append result each)))))
	(sort (main 0 2 nil) (lambda (x y) (< x y)))))


(print (time (main 30)))

