
(quicklisp:quickload :euler.util.my)
(use-package :euler.util.my)



(defun d (n)
  (- (sum (enumdiv n)) n))

(defun ami? (a b)
  (and (/= a b) (= (d a) b) (= (d b) a) ))


(defun amisearch (until &optional (acc 1) (result nil))
  (if (>= acc until) result
	(let1 b (d acc)
		(if (ami? b acc) (amisearch until (1+ acc) (cons (list acc b) result))
		  (amisearch until (1+ acc) result)))))

  
(remove-duplicates (amisearch 10000) :test #'set-equal?)

;; ((6232 6368) (5020 5564) (2620 2924) (1184 1210) (220 284))

