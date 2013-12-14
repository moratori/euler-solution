
(ql:quickload :euler.util.my)

(import '(euler.util.my:psort
		  euler.util.my:range1-n
		  euler.util.my:group
		  euler.util.my:zip
		  euler.util.my:take))

(use :iterate)

(defun split-string (str ch &optional (f (lambda (x)x)))
  (labels
	((main (str acc  result)
	 	(cond 
		  ((string= str "")
		   (if (string= acc "") 
			 result 
			 (cons (funcall f acc) result)))
		  ((char= ch (char str 0))
		   (main (subseq str 1) 
				 "" 
				 (cons (funcall f acc) result)))
		  (t
			(main (subseq str 1)
				  (concatenate 'string acc (subseq str 0 1))
				  result)))))
	(reverse (main str "" nil))))


(defvar *data* 
  (with-open-file (in "./cipher1.txt" :direction :input)
	(split-string (read-line in) #\, #'parse-integer)))

(defvar *key-value* (range1-n 122 97))

(defun xor (code k)
  (boole boole-xor code k))

(defun decrypt (data k)
  (mapcan 
	(lambda (each3)
	  (mapcar 
		(lambda (x) (apply #'xor x)) (zip each3 k)))
  	;; padding 
	(group (append data '(0 0)) 3)))


(defun max-code (data)
  (iter (for each in data)
		(finding each maximizing (count-if (lambda (x) (= x each)) data))))

(defun decode (data)
  (reduce 
	(lambda (x y)
	  (concatenate 'string x y)) (mapcar 
	(lambda (x) 
	  (string (code-char x))) data)))


(destructuring-bind (a b c) (apply #'zip (group (append *data* '(0 0)) 3))
  (let ((am (max-code a))
		(bm (max-code b))
		(cm (max-code c)))
	(dolist (k [(list x y z) | x <- *key-value* y <- *key-value* z <- *key-value*])
	  (destructuring-bind (x y z) k
		(when (= (xor am x) (xor bm y) (xor cm z))
		  (format t "Key = ~A~%~A~%" k (decode (decrypt *data* k))))))))

;;; (103 111 100)

(print (apply #'+ (take 1201 (decrypt *data* '(103 111 100)))))


