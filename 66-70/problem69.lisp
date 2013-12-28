
(ql:quickload :euler.util.my)
(import '(euler.util.my:euler-phi
		  euler.util.my:erat))
(use :iterate)


(time (erat 1000001))

(print (iter (for n from 2 to 1000000)
			 (finding n maximizing (/ n (euler-phi n)))))
