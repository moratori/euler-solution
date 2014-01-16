
(use :euler.util.my)


(defun join (templ tar)
  (append1 
	(flatten (zip templ tar)) 
	(lastelm templ)))

(defun make (n)
  (let1 split (getdigit n)
	(digit->num
	  (join 
	  '(1 2 3 4 5 6 7 8 9 0)
	  (append 
	  	(make-list (- 9 (length split)) :initial-element 0) 
	  	split)))))


(print 
  (sqrt 
	(loop named exit 
		for x from 0
		upto 999999999
		do
		(let1 num (make x)
			  (when (square? num)
				(print num)
				(return-from exit num))))))


