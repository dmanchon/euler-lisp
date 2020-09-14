(defun update-node (x y arr)
	   (+ (aref arr y x)
	      (if (zerop x) 0 (aref arr y (1- x))) 
	      (if (zerop y) 0 (aref arr (1- y) x))))

(defun euler-15 (&key (size 20))
  (let* ((array-size (list (1+ size) (1+ size)))
	 (board (make-array array-size :initial-element 0)))
    (setf (aref board 0 0) 1)
    (loop for y from 0 to size do 
	 (loop for x from 0 to size do 
	      (setf (aref board y x) (update-node x y board))) finally 
	 (return (aref board size size)))))

(time (print (euler-15)))