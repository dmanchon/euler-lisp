(defun triangle (n)
  "Calcula el numero triangular n"
  (/ (+ (* n n) n) 2))

(defun factors (n)
  "Calcula los factores primos, incluyendo repeticiones"
  (labels ((helper (n div)
             (cond ((= n 1) nil)
                   ((zerop (mod n div)) (cons div (helper (/ n div) 2)))
                   (t (helper n (1+ div))))))
           (helper n 2)))

(defun count-uniques (l)
  (mapcar #'(lambda (x) (count x l)) (remove-duplicates l)))

(defun num-factors (n)
  "Esta formula la saque de la wikipedia"
  (reduce #'* (mapcar #'1+ (count-uniques (factors n)))))


(defun euler-12 (&key (divisors 500))
  (loop for i from 2 when (> (num-factors (triangle i)) divisors) collect (return (triangle i))))

(time (print (euler-12)))


;;investigations


(defun count-uniques-old (l)
  "Cuenta las ocurrencias de un mismo elemento en una lista previamente ordenada"
  (labels ((helper (l elem count)
             (cond ((equal (car l) elem) (helper (cdr l) elem (1+ count)))
                   ((null l) (append (list count)))
                   (t (cons count (helper l (car l) 0))))))
    (helper l (car l) 0)))

(defun num-factors-slow (n)
  (loop for i from 1 to n when (zerop (mod n i)) count i))

(defun range (n)
  (loop for i from 1 to n collect i))
 
(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
              val
            (setf (gethash args cache)
                  (apply fn args)))))))

;;(setq num-factors-fast (memoize #'num-factors-slow))


;;very smart, from internet, anyway i slower that the one used
(defun list-divisors (n) 
  (loop for i from 1 to (sqrt n) 
        when (= (mod n i) 0) 
        collect i 
        and unless (= i (/ n i)) 
        collect (/ n i))) 
