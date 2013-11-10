

(load "~/.sbclrc")
(quicklisp:quickload :euler.util.my)
(use-package :euler.util.my)



(print (sum (getdigit (expt 2 1000))))
