
(use :euler.util.my
	 :lisp-unit)



(defconstant +ROMAN-TABLE+
	'((#\I 1)
	  (#\V 5)
	  (#\X 10)
	  (#\L 50)
	  (#\C 100)
	  (#\D 500)
	  (#\M 1000)))

;; (1 4 5 9 10 40 50 90 100 400  500 900 1000)
(defconstant +ARBIC-TABLE+	 
	'((1 "I")
	  (4 "IV")
	  (5 "V")
	  (9 "IX")
	  (10 "X")
	  (40 "XL")
	  (50 "L")
	  (90 "XC")
	  (100 "C")
	  (400 "CD")
	  (500 "D")
	  (900 "CM")
	  (1000 "M")))

(defconstant +ARBIC+ '(1 4 5 9 10 40 50 90 100 400 500 900 1000))


(defun readlines (name)
 (with-open-file (handle name :direction :input) 
  (loop for line = (read-line handle nil nil)
			   while line collect (string-trim '(#\Space #\Newline #\Return) line))))

(defun ref-roman (key)
  (if (char= key #\Null) INF+
 	(cadr (assoc key +ROMAN-TABLE+ :test #'char=))))

(defun ref-arbic (key)
  (cadr (assoc key +ARBIC-TABLE+ :test #'=)))


(defun scdr (str) (subseq str 1))

(defun roman2arabic (roman-str)
  (label 
	(main (roman-str before-char result)
		(if (string= roman-str "") result 
		  (let* ((head-char (char roman-str 0))
				 (hc-num (ref-roman head-char))
				 (bc-num (ref-roman before-char)))
				 (main (scdr roman-str)
						head-char
						(if (< bc-num hc-num)
							(+ (- result bc-num) (- hc-num bc-num))
							(+ result hc-num))))))
	(main roman-str #\Null 0)))


(defun arbic2roman (num)
 (if (zerop num) ""
   (if (member 
		num 
		+ARBIC-TABLE+ :test (lambda (x y) (= x (car y))))
		(ref-arbic num)
		(let1 pos (position-if (lambda (x) (> x num)) +ARBIC+)
			  (if (null pos)
				(concatenate 'String "M" (arbic2roman (- num 1000)))
				(concatenate 'string  (ref-arbic  (nth (1- pos) +ARBIC+)) (arbic2roman (- num (nth (1- pos) +ARBIC+))))
				)
		)
	)
   ) 
  )



(defun main (lis)
  (mapcar 
	(lambda (x)
	  (let1 len (- (length x) (length (arbic2roman (roman2arabic x))))
			(if (> 0 len)
			  (error "oops!! my function's is long!?!?")
			  len))) lis))



(print 
  (sum (main (readlines "roman.txt")))
  )

#|
(define-test roman-to-arbic
	(loop for x from 1 upto 3999 do
		  (assert-equal x (roman2arabic (format nil "~@R" x)))))

(define-test arbic-to-roman
	(loop for x from 1 upto 100000 do
		(assert-equal x (roman2arabic (arbic2roman x)))))


|#
