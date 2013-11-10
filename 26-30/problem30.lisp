

(use :euler.util.my)


(defun nth-power? (n &optional (index 5))
  (and (/= n 1)
	   (= n (sum  
			  (mapcar 
			  (pa$ (flip #'expt) index)
				(getdigit n))))))

;; なんで2000000まででおｋなのかぶっちぇけわからないけど
;; うまくいっちゃった

(print (sum [x | x <- (range1-n 2000000) (nth-power? x 5)]))
