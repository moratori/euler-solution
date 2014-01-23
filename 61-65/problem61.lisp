

(use :euler.util.my)


;; 四桁の数となる n の範囲をそれぞれ示してある


;; 45 <= n <= 140
(defun p3 (n)
  (* 1/2 n (1+ n)))

;; 32 <= n <= 99
(defun p4 (n)
  (* n n))

;; 26 <= n <= 81
(defun p5 (n)
  (* 1/2 n (1- (* 3 n))))

;; 23 <= n <= 70
(defun p6 (n)
  (* n (1- (* 2 n))))

;; 21 <= n <= 63
(defun p7 (n)
  (* 1/2 n (- (* 5 n)  3)))

;; 19 <= n <= 58
(defun p8 (n)
  (* n (- (* 3 n)  2)))


(defconstant +DOMAIN+
			 '((p3 45 140)
			   (p4 32 99)
			   (p5 26 81)
			   (p6 23 70)
			   (p7 21 63)
			   (p8 19 58)))

(defun range (iden)
  (destructuring-bind (s e) (cdr (assoc iden +DOMAIN+))
	(mapcar 
	  (lambda (x)
		(funcall iden x))
	  (range1-n e s))))


(defun head (n)
  (take 2 (getdigit n)))

(defun tail (n)
  (reverse 
	(take 2 (reverse (getdigit n)))))


(defun match? (dom l f)
  (some 
	(lambda (line)
	  (some 
		(lambda (each)
		  (equal l (funcall f each)))
		line)) dom))


(defun cand (iden)
  (let1 dom 
		(mapcar (lambda (x) (range (car x)))
			(remove-if 
			  (lambda (x) (eq (car x) iden)) +DOMAIN+))
	(remove-if-not 
		(lambda (x) 
		  (let* ((head  (head x))
				 (tail  (tail x)))
			(and 
			  (match? dom head #'tail)
			  (match? dom tail #'head))))
		(range iden))))


(defun collect (num useble)
  ;; useble '(p3 p4 p5 p6 ...)
  (mapcar 
	(lambda (x)
	  (list x
			(remove-if-not 
			  (lambda (x)
				(equal (tail num) 
					   (head x))) (cand x)))) useble))

(defun search-it (num first-head  useble result)
  (cond 
	((and (null useble)
		  (equal first-head (tail num))) 
	 (print (cons num result)))
	((null useble)
	 nil)
	(t
	  (dolist (each  (collect num useble))
		(destructuring-bind (iden lst) each
		  (dolist (next lst)
			(search-it next first-head 
					   (remove-if 
						 (lambda (x) (eq x iden)) useble) (cons num result))))))))



(dolist (each (cand 'p8))
  (search-it 
	each 
	(head each) 
	'(p3 p4 p5 p6 p7) nil))



