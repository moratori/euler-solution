
(use :euler.util.my)
(ql:quickload :cl-utilities)
(import '(cl-utilities:split-sequence))

(defun read-lines (filename &optional (f (lambda (x)x)))
  (with-open-file (in filename :direction :input)
	(loop for line = (read-line in nil nil)
		  while line 
		  collect (funcall f (string-trim '(#\Newline #\Return) line)))))


(defconstant +X+ 80)
(defconstant +Y+ 80)

(defconstant +DATA+ 
			 (make-array 
			   (list +X+ +Y+) 
			   :initial-contents
			    (read-lines 
				  "matrix.txt" 
				  (lambda (line) 
					(mapcar 
					  #'parse-integer 
					  (split-sequence #\, line))))))

(defvar *flag* (make-array 
				 (list +X+ +Y+) 
				 :initial-contents 
				 (make-list +Y+ :initial-element (make-list +X+ :initial-element INF+))))


(defstruct (p (:constructor p (x y)))
  			(x 0 :type integer) (y 0 :type integer))



(defconstant +START+ (p 0 0))
(defconstant +END+ (p (1- +X+) (1- +Y+)))


(defmethod checkin (val (p p))
  (setf (aref *flag* (p-y p) (p-x p)) val))

(defmethod getstate ((p p))
  (aref *flag* (p-y p) (p-x p)))

(defmethod point-equal ((p1 p) (p2 p))
 (and 
   (= (p-x p1) (p-x p2))
	(= (p-y p1) (p-y p2))))


(defmethod visitable ((start p))
  (with-slots (x y) start
	(cond 
	  ((point-equal start +END+) nil)
	  ((= x (1- +X+)) 
	   (list (p x (1+ y))))
	  ((= y (1- +Y+))
	   (list (p (1+ x) y)))
	  (t 
		(list (p (1+ x) y)
			  (p x (1+ y)))))))

(defmethod value ((p p))
  (with-slots (x y) p
	(aref +DATA+ (p-y p) (p-x p))))


(defun search-minimal (start &optional (from 0))
  (if (> from (getstate start)) INF+
	(let1 here (value start)
		(checkin from start)
		(if (point-equal start +END+) here
			(let1 way (visitable start)
				  (apply #'min
						 (mapcar 
						   (lambda (next)
							 (+ here (search-minimal next (+ here from)))) way)))))))


(print (search-minimal +START+))
