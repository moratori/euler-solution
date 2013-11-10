

(load "~/.sbclrc")
(quicklisp:quickload :euler.util.my)
(use-package :euler.util.my)

(defun triangle (n) 
  (* 1/2 n (+ n 1)))

;;; この定義だと素朴すぎてだめ
;;; 改良しよう
(defun check (n)
  (if (>= (length (enumdiv (triangle n))) 500) 
	n (check (1+ n))))


(compile 'triangle)
(compile 'check)


(print (triangle 12375))
