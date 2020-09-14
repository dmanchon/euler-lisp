(defun euler-1 (n)
    (if (eql n 0)
	0
	(if (or (eql (mod n 5) 0) (eql (mod n 3) 0))
	    (+ n (euler-1 (1- n)))
	    (euler-1 (1- n)))))
