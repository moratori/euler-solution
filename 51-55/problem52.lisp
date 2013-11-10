
(use :euler.util.my)



(defun consist-same? (a b)
  (set-equal? 
	(getdigit a)
	(getdigit b)))


(defun check? (x)
  (every 
	(pa$ #'consist-same? x) 
	(list (* 2 x) (* 3 x) (* 4 x) (* 5 x) (* 6 x))))


(print (find-fn #'check? 1))
