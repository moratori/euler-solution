
(load "~/.sbclrc")
(quicklisp:quickload :euler.util.my)
(use-package :euler.util.my)



(defun collatz (n len)
  (cond 
	((= n 1) (1+ len))
	((oddp n) 
	 (collatz (+ 1 (* 3 n)) (1+ len)))
	((evenp n) 
	 (collatz (/ n 2) (1+ len)))))

;;; start ~ end 間のどの値で関数を呼べば最大になるか
(defun maximize (func start end &optional (args nil))
  (labels 
	((main (largest-x value test-x)
		   (if (> test-x end) largest-x
			 (let1 now (apply func (cons test-x args))
				   (if (> now value) 
					 (main test-x now (1+ test-x))
					 (main largest-x value (1+ test-x)))))))
	(main nil INF- start)))

(compile 'maximize)
(print (maximize #'collatz 1 999999 '(0)))
