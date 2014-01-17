

(ql:quickload :euler.util.my)
(import 
  '(euler.util.my:find-fn
	euler.util.my:square?
	euler.util.my:div?))

(use :iterate)


#|

	x^2 -Dy^2 = 1 <-> y = sqrt((1/D) * (x^2 - 1))
	xを決めれば自動的に y は決まるけど
	ルートの中身を平方数にするようなxを選ばねばならない
	純粋に考えたらこうだけど 61 でやばいことになる
	う〜ん
	なんかうまい方法があるはず〜

|#


(defun minimal-x (d)
  (find-fn 
	(lambda (x)
	  (square? (* (/ 1 d) (1- (* x x))))) 2))





(print 
  (iter (for d from 2 to 1000)
	  (when (not (square? d))
		(print d)
		(finding d maximizing (minimal-x d)))))


