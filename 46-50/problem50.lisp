
(use :euler.util.my)


(defvar *table* (erat 1000000))


(defun take-some (lst n &optional (result 0) (terms 0))
  (cond
	((= result n) terms)
	((null lst) 0)
	((> (+ (car lst) result) n)  0)
	(t (take-some (cdr lst) n (+ (car lst) result) (1+ terms)) )))



(defun len (prime)
  ; 素数 prime を素数の和で表した時の長さを返す関数
  (let1 tar (take (position prime *table*) *table*)
		(label 
		  (main (use)
				(if (null use) 0
				  (let1 can (take-some use prime)
					(if (zerop can) (main (cdr use)) can))))
		  (main tar))))


(let ((e 0) (max 0))
	  (dolist (each *table*)
		(let1 l (len each)
			  (when (> l max)
				(setq max l
					  e each))))
	  (print e))

