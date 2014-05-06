

(use :euler.util.my)



(defstruct (point (:constructor point (x y)))
  (x 0 :type number)
  (y 0 :type number))


(defstruct (triangle (:constructor triangle (p1 p2 p3)))
  (p1 (error "point required") :type point)
  (p2 (error "point required") :type point)
  (p3 (error "point required") :type point))


;;; N = {0,1,2}
;;; N^2 = {(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),
;;; 	   (2,0),(2,1),(2,2)}


(defmethod distance ((p1 point) (p2 point))
  (with-slots ((x1 x) (y1 y)) p1
	(with-slots ((x2 x) (y2 y)) p2
	  (sqrt
		(+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))))))


(defmethod right-triangle? ((tr triangle))
  (with-slots (p1 p2 p3) tr
	(let ((p12 (expt (distance p1 p2) 2))
		  (p13 (expt (distance p1 p3) 2))
		  (p23 (expt (distance p2 p3) 2)))
	  
	  (or
		(= (print p12) (print (+ p13 p23)))
		(= (print p13) (print (+ p12 p23)))
		(= (print p23) (print (+ p12 p13)))))))




(print 
  (every 
	(lambda (x)
	  (right-triangle? 
		(triangle (point 0 0)
				  (first x)
				  (second x))))
	(list 
	  (list (point 1 0) (point 0 1))
	  ;(list (point 1 0) (point 1 1))
	  ;(list (point 1 0) (point 0 2))
	  ;(list (point 1 0) (point 1 2))
	  ;(list (point 0 1) (point 2 0))
	  ;(list (point 1 1) (point 2 0))

	  )
	))
