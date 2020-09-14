(defun day-of-the-week (day month year)
  (nth-value 6 
	     (decode-universal-time 
	      (encode-universal-time 0 0 0 day month year 0) 0)))

(defun euler-19 ()
  (count 6 (loop for year from 1901 to 2000 append 
		(loop for month from 1 to 12 collect 
		     (day-of-the-week 1 month year)))))

(time (print (euler-19)))