(defun palindromep (num)
  (equalp (prin1-to-string num) (reverse (prin1-to-string num))))

(defun find-factors (min max number)
  (loop for i from max downto min append 
       (loop for j from max downto min until (< (* j i) number) 
	  when (= (* i j) number) collect (list i j))))

(defun euler-4 (&key (min 100) (max 999))
  (labels ((euler-aux (min max number)
	     (cond ((and (palindromep number) (not (null (find-factors min max number)))) number)
		   (t (euler-aux min max (1- number))))))
    (euler-aux min max (* max max))))

(compile 'euler-4)
(time (print (euler-4)))




(defun num-at (num n)
  (digit-char-p (aref (prin1-to-string num) n)))

(defun palindromep-old (num)
  (labels ((palindrome-helper (num n) 
	     (let* ((total-len (1- (length (prin1-to-string num)))) (mirror-len (floor (/ total-len 2))))
	       (cond ((> n mirror-len) t)
		     ((= (num-at num n) (num-at num (- total-len n))) (palindrome-helper num (1+ n)))
		     (t nil)))))
    (palindrome-helper num 0)))


;;just for fun
(defun find-palindromes (limit)
  (labels ((find-helper (limit n)
	     (cond ((= limit n) nil)
		   ((palindromep n) (cons n (find-helper limit (1+ n))))
		   (t (find-helper limit (1+ n))))))
    (find-helper limit 0)))

