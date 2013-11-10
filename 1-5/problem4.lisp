

;(load (compile-file "mod.lisp"))
(load "~/.sbclrc")
(quicklisp:quickload :macro.util.my)
(use-package :macro.util.my)


(defun div (a b) (floor (/ a b)))

(defun getdigit (n)
  (if (> 10 n) 
	(list n)
	(cons (mod n 10) (getdigit (div n 10)))))



(defun palindrome (n) 
  (equal (getdigit n) (reverse (getdigit n))))


(defun main ()
  (car (last (loop for x from 10000 upto 99800 if (palindrome x) collect x))))


(compile 'getdigit)
(compile 'palindrome)
(compile 'main)

(defconstant +domain+ (loop for x from 100 upto 999 collect x))

(print (apply #'max [(* x y) | x <- +domain+ y <- +domain+ (palindrome (* x y))]))



