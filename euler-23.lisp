(defun divisors (n)
  (cons 1 (remove-duplicates 
           (loop for i from 2 to (sqrt n) when (zerop (mod n i))
                 append (list i (/ n i))))))

(defun d (n)
  (reduce #'+ (divisors n)))

(defun is-abundant? (n)
  (when (> (d n) n)
         t))

(defun euler-23 (&key (limit 20162))
  (let ((arr-sum (make-array (1+ limit) :element-type 'bit :initial-element 0))
	(arr-ab (make-array (1+ limit) :element-type 'bit :initial-element 0)))
    (loop for i from 1 to limit when (is-abundant? i) do (setf (bit arr-ab i) 1))
    (loop for i from 1 to limit when  (= 1 (bit arr-ab i)) do
          (loop for j from 1 to limit when (and (< (+ i j) limit)(= 1 (bit arr-ab j))) do
                (setf (bit arr-sum (+ i j)) 1))
          finally (return  (loop for k from 1 to (1- limit) when (= (bit arr-sum k) 0) sum k)))))

(time (print (euler-23)))