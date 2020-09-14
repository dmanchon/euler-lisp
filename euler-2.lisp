(defun fib (n)
  (check-type n (integer 0 *))
  (labels ((fib-aux (n a0 a1)
		  (if (zerop n)
		      a0
		      (fib-aux (1- n) a1 (+ a0 a1)))))
	 (fib-aux n 1 1)))

(defun euler-2 (&key (limit 4000000))
  (labels ((euler-2-sum (n total) 
	     (let ((fib-term (fib n)))
	     (if (> fib-term limit)
		 total
		 (euler-2-sum (1+ n) (+ total (if (evenp fib-term) fib-term 0)))))))
	     (euler-2-sum 0 0)))

(time (euler-2))
  


      
  