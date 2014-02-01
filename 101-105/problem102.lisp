

(use :euler.util.my)


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
	  (/ (- y2 y1) (- x2 x1)))))


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



