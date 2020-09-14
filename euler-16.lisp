(defun euler-16 (&key (n 1000))
  (loop for char across (prin1-to-string (expt 2 n)) sum 
       (digit-char-p char)))

(defun euler-16b (&key (n 1000))
  (reduce #'+ (map 'list #'digit-char-p (prin1-to-string (expt 2 n)))))

(time (print (euler-16)))