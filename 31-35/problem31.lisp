
(use :euler.util.my)

;; FPってなにおいしいの?

(let1 cnt 0
	(for (one 0) (<= one 200) (1+ one) cnt
		 (for (two 0) (<= two 100) (1+ two) nil
			  (for (five 0) (<= five 40) (1+ five) nil
				   (for (ten 0) (<= ten 20) (1+ ten) nil
						(for (tw 0) (<= tw 10) (1+ tw) nil
							 (for (fif 0) (<= fif 4) (1+ fif) nil
								  (for (hun 0) (<= hun 2) (1+ hun) nil
									   (when 
										 (= 
										   (+ one 
											  (* 2 two) 
											  (* 5 five) 
											  (* 10 ten) 
											  (* 20 tw) 
											  (* 50 fif) 
											  (* 100 hun)) 200) (incf cnt)))))))))
	(print (1+ cnt)))

