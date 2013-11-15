
(use :euler.util.my)


;;; 1 <= b <= 9しかありえない
;;; 10とかだったら一回ずつ桁があがる
;;; 10^1 = 10
;;; 10^2 = 100
;;; 10^3 = 1000
;;; 明らかに追いつかない
;;; また
;;; b^mの桁数 < m -> b^(m+1)の桁数 < m+1のはず
;;; だからこれでいい
(defun num (n &optional (acc 1) (result 0))
  (let1 len  (length (getdigit(expt n acc)))
	(if (< len acc) result
	  (if (= len acc) 
		(num n (1+ acc) (1+ result))
		(num n (1+ acc) result)))))


(print (loop for x from 1 upto 9 sum (num x)))

;(print (num 7))
