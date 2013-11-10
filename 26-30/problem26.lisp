
(use :euler.util.my)


;; (0 1 4 2 8 5 7  1 4 2 8 5 7  1 4 2 8 5 7)
;; (0 2 2 7 8  2 2 7 8  2 2 7 8  2 2 7 8)
(defun circling? (lst)
  nil
  )

;; (getdecimal 1/7) -> ( 1 4 2 8 5 7 ~)
;; (getdecimal 1/5) -> ( 2 -)
(defun getdecimal (ratn &optional (result nil))
  (sleep 1)
  (format t "result = ~A~%" result)
  (multiple-value-bind (upper lower) (floor ratn)
	(cond
	  ((circling? result))
	  ((zerop lower) 
	   (reverse (cons '- (cons upper result))))
	  (t 
		(getdecimal (* lower 10) (cons upper result))))))

(print (getdecimal  ))
