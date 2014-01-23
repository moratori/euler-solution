

(use :euler.util.my)
(ql:quickload :cl-ppcre)
(import '(cl-ppcre:split))

(defun readata (fname)
  (with-open-file (in fname :direction :input)
	(loop for line = (read-line in nil nil)
		  while line 
		  collect 
		  	(mapcar #'parse-integer 
					(split " " (string-trim '(#\Return #\Space #\Newline) line))))))


(defconstant +DATA+ (readata "triangle.txt"))


(defvar *hash* 
  (make-hash-table :test #'equal))

(defun x (p) 
  (first p))

(defun y (p) 
  (second p))

(defun getnum (p) 
  (nth (1- (y p)) 
	   (nth (1- (x p)) +DATA+)))

(defun getnexcan (pos)
  (let ((nextx (+ 1 (x pos))))
		(list 
		  (list nextx (y pos))
		  (list nextx (1+ (y pos))))))



(defun max-path (start-pos end-line &optional (acc 0)) 
  (let ((val (+ acc (getnum start-pos)))
		(there (gethash start-pos *hash*)))

	(when (or (null there) (> val there))
	  (setf (gethash start-pos *hash*) val))

	(if (> (gethash start-pos *hash*) val) INF-
	  (cond
	  	((= end-line (x start-pos)) 
		 val)
		(t (apply #'max 
				  (mapcar 
					(lambda (x) 
					  (max-path x end-line val)) 
					(getnexcan start-pos))))))))


(print (time (max-path '(1 1) (length +DATA+))))

