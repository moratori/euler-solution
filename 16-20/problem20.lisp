

(load (compile-file "mod.lisp"))

(defun fact (n &optional (acc 1))
  (if (zerop n) acc (fact (1- n) (* n acc))))

(compile 'fact)

(print (sum (getdigit (fact 100))))
