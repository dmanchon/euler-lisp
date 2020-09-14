(defun test-cond (a b c)
	(= (+ a b c) 1000))

(defun entero? (n)
  (= n (floor n)))

(defun calc-c (a b)
  (sqrt (+ (* a a) (* b b))))

(defun gen-triplets (limit) 
	(loop for i from 1 to limit append 
		(loop for j from 1 to i when (entero? (calc-c i j)) 
			collect (list i j (floor (calc-c i j))))))

(defun euler-9 (n)
  (reduce #'* (car (remove-if-not #'(lambda (x) (if (apply #'test-cond x) x)) (gen-triplets n)))))

(time (print (euler-9 1000)))
