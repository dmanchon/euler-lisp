(defun primep (n)                                                    
           (labels ((is-prime-aux (n div)                                       
                      (if (or (eq div n) (eq n 1))                              
                          t                                                     
                          (if (eq (mod n div) 0)                                
                              nil                                    
                              (is-prime-aux n (1+ div))))))                     
             (is-prime-aux n 2)))		 

(defun euler-7b (n)
  (do ((num 1 (+ num 1))                                                                                                                                 
       (couter 0 counter))                                                                                                                               
      ((> y n) (1- num))                                                                                                                     
    (if (primep num) (incf counter))))

;;better style
(defun euler-7 (n)
  (do ((num 0 (+ num 1))                                                                                                                                
       (counter 0 (if (primep num) (1+ counter) counter)))                                                                                              
      ((> counter n) (1- num))))




;; is not working for large numbers, i dont know why
(defun euler-7r (limit)
  (labels ((euler-aux (limit counter n)
	     (cond ((and (primep n) (= counter limit)) n)
		   ((primep n) (euler-aux limit (1+ counter) (1+ n)))
		   (t (euler-aux limit counter (1+ n))))))
    (euler-aux limit 1 2)))	     

(compile 'euler-7)

;; (loop for i from 1 to 100 when (primep i) collect i)