(defun primep (n)                                                    
           (labels ((is-prime-aux (n div)                                       
                      (if (or (eq div n) (eq n 1))                              
                          t                                                     
                          (if (eq (mod n div) 0)                                
                              nil                                    
                              (is-prime-aux n (1+ div))))))                     
             (is-prime-aux n 2)))		 

(defun euler-3 (&key (n 600851475143))
  (labels ((euler-aux (n div)
	     (if (and (zerop (mod n div)) (primep div))
		 div
		 (euler-aux n (1- div)))))
    (euler-aux n (floor (sqrt n)))))

(defun euler-3b (&key (n 600851475143))
  (labels ((euler-aux (n div)
	     (cond ((= div 0) nil)
		   ((and (zerop (mod n div)) (primep div)) div)
		   (t  (euler-aux n (1- div))))))
    (euler-aux n (floor (sqrt n)))))

;; bajado de internet
(defun primfac (num) 
  (when (> num 1) 
    (do ((x 2 (1+ x))) 
	((zerop (mod num x)) 
	 (cons x (primfac (/ num x)))))))

;; for tail recursion opt ??
(compile 'euler-3b)
(compile 'euler-3)


(time (print (euler-3)))