(defun fact (n)
  (if (= n 1) 
      1 
      (* n (fact (1- n)))))

(defun euler-20 (&key (n 100))
  (loop for char across (prin1-to-string (fact n)) sum
       (digit-char-p char)))

(time (print (euler-20)))