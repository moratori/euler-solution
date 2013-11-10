
(quicklisp:quickload :euler.util.my)
(use-package :euler.util.my)

(defconstant 
  +unit+ '((2 hundred) 
		   (3 thousand)))

(defconstant 
  +direct-strexp+ 
  			'((1 one)
			 (2 two)
			 (3 three)
			 (4 four)
			 (5 five)
			 (6 six)
			 (7 seven)
			 (8 eight)
			 (9 nine)
			 (10 ten)
			 (11 eleven)
			 (12 twelve)
			 (13 thirteen)
			 (14 fourteen)
			 (15 fifteen)
			 (16 sixteen)
			 (17 seventeen)
			 (18 eighteen)
			 (19 nineteen)))

(defconstant +digit2+ 
			 '((2 twenty)
			   (3 thirty)
			   (4 forty)
			   (5 fifty)
			   (6 sixty)
			   (7 seventy)
			   (8 eighty)
			   (9 ninety)))


(defun aces (key alst)
  (symbol-name (cadr (assoc key alst))))


;;; もうくそ適当
;;; ゴミコード
;;; こういうのだいきらいだ
(defun convert (n)
  (let1 target (getdigit n)
		(cond 
		  ((> 20 n) 
		   (aces n +direct-strexp+))
		  ((> 100 n) 
		   (if (= 0 (first target))
			 ""
			 (if (= 0 (second target))
			 (aces (first target) +digit2+)
			 (concatenate 'string
			 	(aces (first target) +digit2+) 
			 	(aces (second target) +direct-strexp+)))))
		  (t 
			(if (div? n 100) 
			  (concatenate 'string (aces (first target) +direct-strexp+) "hundred")
			  (concatenate 'string 
						 (aces (first target) +direct-strexp+) "hundredand" 
						 (convert (+ (* 10 (second target)) (third target)))))))))

(print (+ 11 (sum (mapcar (lambda (x) (length (print (convert x)))) (range1-n 999)))))




