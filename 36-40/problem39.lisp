
(use :euler.util.my)


;;; P: a,b,c>0 and a+b+c = p and a^2 = b^2 + c ^2 -> b + c > a
;;; notPを仮定
;;; notP : a,b,c>0 and a+b+c = p and a^2 = b^2 + c^2 -> b + c <= a
;;; a = sqrt(b^2 + c^2)
;;; b+c<= sqrt(b^2+c^2)
;;; (b+c)^2 <= b^2 + c^2
;;; b,c>0 より矛盾
;;; よって b + c > a ■
;;; p - a > a
;;; 0 < a  < p -a
;;; a < 2a < p
;;; 2a < p
;;; a < p/2
;;;


#|
(defun srch (p)
  (length 
	[(list a b c) | 
		a <- (range1-n (floor (/ p 2))) b <- (range1-n (1- a)) c <- (range1-n b)
		(and (= (+ a b c) p) (= (* a a) (+ (* b b) (* c c))))]))
|#

;;; ほとんど全探索っぽい感じ
;;; 少なくとも純粋verよりは倍くらい速い
(defun srch (p)
  (let ((result 0))
	(do ((a 1 (1+ a))) ((>= a (/ p 2)) result)
	  (do ((b 1 (1+ b))) ((>= b a) nil)
		(do ((c 1 (1+ c))) ((>= c b) nil)
		  (when (and (= p (+ a b c)) (= (* a a) (+ (* b b) (* c c))))
			(incf result)))))))


(compile 'srch)

(print (time (maximize #'srch 3 1000)))
