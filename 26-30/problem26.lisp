


#|
	

	 1 / 210 = 0...1
    10 / 210 = 0...10
   100 / 210 = 0...100
  1000 / 210 = 4...160
  1600 / 210 = 7...130
  1300 / 210 = 6...40
   400 / 210 = 1...190
  1900 / 210 = 9...10
 
      *                 *
  (1 10 100 160 130 40 190 10)

  おんなじあまりが出てきたらあきらかにそのあとは循環

|#


(use :euler.util.my)

(defun cyclen (d) 
  (labels 
	((main (acc lst)
	  (let* ((r (mod acc d)) 
			 (i (position r lst)))
		(if (not (null i))
		  (- (length lst) i)
		  (main (* r 10)  (append1 lst r)))))) (main 1 nil)))


(print (maximize #'cyclen 1 999))

