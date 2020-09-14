(defun divisors (n)
  (cons 1 (remove-duplicates 
           (loop for i from 2 to (sqrt n) when (zerop (mod n i))
                 append (list i (/ n i))))))

(defun d (n)
  (reduce #'+ (divisors n)))

(defun get-amicable-number (a)
  (let ((b (d a)))
    (when (and (not (= a b)) (= (d b) a))
      (list a b))))

(defun euler-21 (&key (n 10000))
  (reduce #'+ (remove-duplicates 
	       (loop for i from 1 to (1- n) append 
		    (get-amicable-number i)))))

(time (print (euler-21)))