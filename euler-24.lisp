;; http://en.wikipedia.org/wiki/Factoradic

(defun ! (n)
  (labels ((helper (n acc)
             (cond ((= n 0) acc)
                   (t (helper (1- n) (* acc n))))))
    (helper n 1)))

(defun factoradic (n base)
  (labels ((helper (n base i j)
	     (assert (> (! (1+ base)) n) (n base) "Number is to big for that base")
	     (cond ((= n 0) (append (loop for zeros from i downto 0 append (list 0))))
		   ((< n (* (! i) j)) (append 
				       (list (1- j))
				       (helper (- n (* (! i) (1- j))) base (1- i) 0)))
		   (t (helper n base i (1+ j))))))
    (helper n base (1- base) 0)))

(defun euler-24 (&key (n 999999) (base 10))
  (let ((symbols (loop for i from 0 to (1- base) collect i))
	(factoradic (factoradic n base)))
    (labels ((helper (l-fact l-sym)
	       (cond ((null l-fact) nil)
		     (t (append 
			 (list (nth (car l-fact) l-sym))
			 (helper (cdr l-fact) (remove (nth (car l-fact) l-sym) l-sym)))))))
      (helper factoradic symbols))))

(time (print (euler-24)))

