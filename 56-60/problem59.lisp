
(use :euler.util.my)

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


(print 
  (psort
	   (remove-duplicates(mapcar 
		 (lambda (x)
		   (list x (count x *data*))) *data*) :test #'equal)
	   (lambda (x y)
		 (> (second x) (second y)))))


;(print 
 ; (find-fn 
;	(lambda (x) (= 101 (boole boole-xor 79 x))) 43))

;; e/E
;; 101 -> 79
;; 69  -> 79
#|
(print (mapcar 
		 (lambda (x)
		   (code-char (boole boole-xor x 10))) *data*))
 E?       T?       A?
(79 86) (68 77) (71 70)

XOR()

|#

(defun concat (cl)
  (reduce (lambda (x y) (concatenate 'String x (string y))) cl :initial-value ""))



(defun f (a x y z)
  (boole boole-xor (boole boole-xor (boole boole-xor a x) y) z))



(mapc (lambda (x) (format t "~A~%~%" x))  [(concat (mapcar (lambda (x) (code-char (boole boole-xor x key)))  *data*))| key <- (range1-n 366 291)])

