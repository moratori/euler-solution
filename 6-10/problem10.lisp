(load "~/.sbclrc")
(quicklisp:quickload :euler.util.my)
(use-package :euler.util.my)


(print (sum (erat 2000000)))
