
(use :euler.util.my)

(defun get-column (p data &optional (f 'c))
  (loop for i from 0 upto 8
		collect (if (eq f 'c)
					(aref data i (car p))
					(aref data (car p) i))))

(defun get-row (p data)
  (get-column (reverse p) data 'r))

(defun get-block (p data)
  (mapcar 
	  (lambda (each-p) 
		(apply (pa$ #'aref data) each-p))
	  [(list (+ y (* (div (second p) 3) 3)) 
			 (+ x (* (div (car p) 3) 3))) 
	   		| x <- (range1-n 2 0) 
			  y <- (range1-n 2 0)]))

(defun can (p data)
  (set-difference 
	'(1 2 3 4 5 6 7 8 9)
	(append
	  (get-column p data)
	  (get-row p data)
	  (get-block p data))))

(defun find-void (data)
  (block exit
	(for (i 0) (< i 9) (1+ i) nil
		(for (j 0) (< j 9) (1+ j) nil
			 (when (zerop (aref data i j))
			   (return-from exit (list j i)))))))

(defun show-board (data)
  (for (i 0) (< i 9) (1+ i) nil
	   (for (j 0) (< j 9) (1+ j) nil
			(format t "~A " (aref data i j)))
	   (if (div? (1+ i) 3)
		 (format t "~%~%")
		 (format t "~%"))))

(defun get3digit (data)
  (digit->num 
	(list 
	  (aref data 0 0)
	  (aref data 0 1)
	  (aref data 0 2))))

(defun split (str) 
  (if (string= str "") nil
	(cons (parse-integer (subseq str 0 1)) (split (subseq str 1)))))

(defun read-sudoku (name)
  (with-open-file 
	(handle name :direction :input :if-does-not-exist nil)
		(let1 result nil
			(for (i 0) (< i 50) (1+ i) nil
			(let1 each nil
				(for (i 0) (< i 10) (1+ i) nil
					(let1 line (read-line handle)
						(when (/= i 0)
						  (push (split line)  each))))	  
					(push (reverse each) result))) 
			(mapcar (lambda (x) (make-array '(9 9) :initial-contents x)) (reverse result)))))

;; ! durty function !
;; solve and print sum of the 3-digit numbers found in the top left corner
(defun solve-sudoku (data) 
  (let1 void-p (find-void data)
		(if (null void-p) (print (get3digit data))
		  (let1 cand (can void-p data)
				(dolist (each cand)
				  (setf (aref data (cadr void-p) (car void-p)) each)
				  (solve-sudoku data)
				  (setf (aref data (cadr void-p) (car void-p)) 0))))))



(mapc #'solve-sudoku (read-sudoku "./sudoku.txt"))
