

(use :euler.util.my)
(ql:quickload :cl-ppcre)
(import '(cl-ppcre:split))


(defstruct (point (:constructor point (x y)))
  (x 0 :type number)
  (y 0 :type number))


(defstruct (triangle (:constructor triangle (p1 p2 p3)))
  (p1 (error "point required") :type point)
  (p2 (error "point required") :type point)
  (p3 (error "point required") :type point))


(defmethod change-rate ((p1 point) (p2 point))
  (with-slots ((x1 x) (y1 y)) p1
	(with-slots ((x2 x) (y2 y)) p2
	  (when (zerop (- x2 x1)) (print x2))
	  (/ (- y2 y1) (- x2 x1)))))

;;; 直角三角形だと定数関数だ?
(defmethod line ((p1 point) (p2 point))
  (with-slots ((x1 x) (y1 y)) p1
	(let* ((a (change-rate p1 p2))
		   (b (- y1 (* a x1))))
		(values 
			(lambda (x) (+ (* a x) b))
			(point (/ (* -1 b) a) 0) ;; with x
			(point 0 b)              ;; with y
			))))


;;; あ、あれ
;;; 三角形が点を内包するかどうかの判定って
;;; スマートにどうやればいいんだっけ...
;;; 思いついたのは、
;;; 
;;; 三角形を構成する三点 p1,p2,p3について
;;; p1 - p2 , p2 - p3 , p3 - p1 の3つの線分で
;;; 合計4つの交点をx軸と２つy軸と２つもち、
;;; x軸の２つの交点を結ぶ線分とy軸とが直交する
;;; 
;;; これで必要十分?


(defmethod check ((target point) (start point) (end point))
  (with-slots ((tx x)) target
	(with-slots ((sx x)) start
	  (with-slots ((ex x)) end
		(let1 range (sort (list sx ex) (lambda (x y) (> x y)))
			(apply #'> (list (car range) tx (second range))))))))

(defmethod getintersect ((tr triangle))
  (with-slots (p1 p2 p3) tr
	(multiple-value-bind (func1 w-x1 w-y1) (line p1 p2)
	  (declare (ignore func1))
	  (multiple-value-bind (func2 w-x2 w-y2) (line p2 p3)
		(declare (ignore func2))
		(multiple-value-bind (func3 w-x3 w-y3) (line p3 p1)
		  (declare (ignore func3))
		  (append
			 (remove-if-not 
				(lambda (x) (check x p1 p2)) (list w-x1 w-y1))
		  	 (remove-if-not 
				(lambda (x) (check x p2 p3)) (list w-x2 w-y2))
		  	 (remove-if-not 
				(lambda (x) (check x p3 p1)) (list w-x3 w-y3))))))))


(defmethod such-triangle? ((tr triangle))
  (let1 points (getintersect tr)
	(if (not (= 4 (length points))) nil
	  (let1 tar 
			(remove-if-not
			  (lambda (x) 
				(zerop (point-y x)))points)
			(if (not (= 2 (length tar))) nil
			  (let ((x1 (point-x (first tar)))
					(x2 (point-x (second tar))))
					(destructuring-bind (a b) (sort (list x1 x2) (lambda (x y) (> x y)))
					  (and (> a 0) (> 0 b)))))))))

;;; 3
(with-open-file (in "triangles.txt" :direction :input)
  (print 
	(loop for line = (read-line in nil nil nil)
		while line 
		count (ignore-errors
				(such-triangle? 
				(apply #'triangle (mapcar (lambda (x) 
						  (apply #'point x)) 
						(group 
						  (mapcar #'parse-integer (split "," line)) 2))))
				))))
