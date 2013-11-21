(use :euler.util.my)

(print (apply #'max [(sum (getdigit (expt a b))) | a <- (range1-n 99) b <- (range1-n 99)]))
