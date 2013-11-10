(defmacro filter (var pred lst)
  `(remove-if-not 
	 (lambda (,var) ,pred) ,lst))

(defmacro let1 (var expr &rest body)
  `(let ((,var ,expr)) ,@body))

(defun append1 (lst elm) 
  (append lst (list elm)))

(defun range1-n (n &optional (func (lambda (x)x)) (result nil))
  (if (zerop n) 
	result 
	(range1-n (1- n) func (cons (funcall func n) result))))

(defun take (n lst) 
  (subseq lst 0 n))

(defun lastelm (lst) 
  (car (last lst)))

(defun div? (a b) 
  (zerop (mod a b)))

(defun div (a b)
  (floor (/ a b)))

(defun sum (lst) 
  (apply #'+ lst))

(defun prod (lst) 
  (apply #'* lst))

(defun erat (n)
  (if (< n 2) nil
	(let1 finval (sqrt n)
	  (labels 
	    ((main (result target)
		   (let1 head (car target)
			  (if (> head finval) 
			  	(append result target)
			  	(main 
					(append1 result head) 
					(filter x (not (div? x head)) target))))))
		(main nil (cdr (range1-n n)))))))


;;; 各桁の数を取り出す
(defun getdigit (n &optional (result nil))
  (if (< n 10) 
	(cons n result)
	(getdigit (div n 10) (cons (mod n 10) result))))


;;; .  n < 2 であるなら素数の定義より明らかに素数でない
;;; .  2 は　最小の素数である
;;; .  2 < n かつ n が偶数なら素数でない(全ての偶数は2で割れる)
;;; 残る素数の対象は奇数のみである. 奇数は偶数で割ることはできない
;;; よって試し割りは奇数で試していけばよい
;;;
;;; ある数 n が合成数なら n = p1 * p2 * ... * pm と表すことができる(算術の基本定理)
;;; ここでどの pi も pi^2 > n と仮定する(背理法でひっくり返しあるpiは pi^2 <= n を満たすとしたい)
;;; 前述の式は n^2 = p1^2 * p2^2 * ... * pm^2 とも表せるが
;;; 仮定より,明らかにこの積はn^2より大きくなる. よってあるpiはpi^2 <= n である
(defun prime? (n) 
  (cond 
	((< n 2) nil)
	((= n 2) t)
	((div? n 2) nil)
	(t 
	  (let1 fin (sqrt n)
			(labels 
			  ((main (acc) 
					 (cond 
					   ((> acc fin) t)
					   ((div? n acc) nil)
					   (t (main (+ acc 2)))))) (main 3))))))

