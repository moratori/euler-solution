
(load (compile-file "mod.lisp"))



(defun binsearch (target start end)
  (let* ((now (+ (div (- end start) 2) start) ) (val (length (erat now))))
		(cond 
		   ((= val target) now)
		   ((> val target) (binsearch target start now))
		   (t (binsearch target now end)))))


(print (last (erat (binsearch 10001 2 200000))))
