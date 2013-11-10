
(quicklisp:quickload :euler.util.my)
(use-package :euler.util.my)


(defun leap? (year)
  (cond 
	((and (not (div? year 400)) (div? year 100))
	 nil)
	((div? year 4)
	 t)
	(t nil)))


(defun mdayof (year month)
  (cond 
	((= month 2)
	 (if (leap? year) 29 28))
	((member month '(9 4 6 11))
	 30)
	(t 31)))

(defun ydayof (year)
  (labels 
	((main (acc result)
		   (if (> acc 12) result
			 (main (1+ acc) (+ (mdayof year acc) result)))
		   ))(main 1 0)))

;; start の 1/1 から end の12/31まで何日あるか
(defun serialday (start end)
  (loop for x from start upto end sum (ydayof x)))

;; 366以下になるまで年で引き算する
;; その時の年が対象年であり、その時の番号が
;; 年の中での番号
;; start はまあ 呼び出し元は1901で
(defun subsyear (start num)
  (if (>= (ydayof start) num) (values start num)
	(subsyear 
	  (1+ start) 
	  (- num (ydayof start)))))

;; start はまあ 呼び出し元は1で
(defun subsmonth (year start num)
    (if (>= (mdayof year start) num) 
	  (progn  (values start num))
	(subsmonth 
	  year
	  (1+ start)
	  (- num (mdayof year start)))))

(defun ymd (start num)
  (multiple-value-bind (year num) (subsyear start num)
	(multiple-value-bind (month day) (subsmonth year 1 num)
		(list year month day))))

(defun start-with-sunday? (num)
  (let1 tmp (ymd 1900 num)
	(if (and (div? num 7) (= 1 (third tmp)))
	  t nil)))

(defun main ()
  (let1 serial (serialday 1901 2000)
	(length 
  		(loop for x from 366
		  	  upto (+ serial 365)
		      if (start-with-sunday? x) collect x))))

(print (main))
