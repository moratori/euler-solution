

(load "~/.sbclrc")
(quicklisp:quickload :euler.util.my)
(use-package :euler.util.my)

(defconstant +d+ (range1-n 1000))


(defun special-pyt? (a b c)
  (and (< a b c)  (= (+ (* a a) (* b b)) (* c c)) (= (+ a b c) 1000)))


(defun main () 
  [(* a b c) | a <- +d+ b <- +d+ c <- +d+ (and (< a b c) (= (+ (* a a) (* b b)) (* c c)) (= (+ a b c) 1000)) ])


(compile 'special-pyt?)
(compile 'main)

(print (main))
