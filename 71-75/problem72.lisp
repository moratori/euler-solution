
(use :euler.util.my)




(defun solve (n)
  (loop for x from 2 upto n sum (euler-phi x)))



(print 
  (solve 1000000))



