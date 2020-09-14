;; the new version with improved loop and custom memoization runs 5 times faster

(defvar *cache* (make-hash-table :test #'equal))

(defun collatz (n)
  (if (evenp n) 
      (/ n 2) 
    (+ (* 3 n) 1)))

(defun collatz-serie (n)
  (labels ((helper (n len) 
             (multiple-value-bind (val stored?) (gethash n *cache*)
               (if stored? 
                   val
                 (setf (gethash n *cache*) (cond ((= n 1) len)
                                                 (t (+ len (helper (collatz  n) len)))))))))
    (helper n 1)))

;; learning how to loop
(defun euler-14 (&key (n 1000000))
  (loop with max = 0 and pos = 0 
        for i from n downto 1 
        when (> (collatz-serie i) max) 
        do (setf max (collatz-serie i)) and do (setf pos i) 
        finally (return (list max pos))))

(time (print (euler-14)))
;;----------------------------------------------------------------------------------------


(defun collatz-serie-old (n)
  (cond ((= n 1) (list 1))
	((evenp n) (cons n (collatz-serie-old (/ n 2))))
	(t (cons n (collatz-serie-old (+ (* 3 n) 1))))))

(defun collatz-serie-len (n)
  (length (collatz-serie-old n)))

;;here is the bottle-neck, performace is very poor creating the array...
(defun gen-series-pairs (n)
  (loop for i from 1 to n collect 
       (list (collatz-serie-len i) i)))

(defun euler-14-old (&key (n 1000000))
  (car (sort (gen-series-pairs n) #'(lambda (x y) (> (car x) (car y))))))

;; INVESTIGATIONS

;; this is actually very nice
(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
              val
            (setf (gethash args cache)
                  (apply fn args)))))))


;;(setf (fdefinition 'collatz-serie) (memoize #'collatz-serie))

(defun collatz-serie-memo (n)
  (multiple-value-bind (val stored?) (gethash n *cache*)
    (if stored? 
        val
      (setf (gethash n *cache*) (cond ((= n 1) (list 1))
                                    ((evenp n) (cons n (collatz-serie (/ n 2))))
                                    (t (cons n (collatz-serie (+ (* 3 n) 1)))))))))


(defun collatz-serie-first (n)
  (labels ((helper (n len) 
             (multiple-value-bind (val stored?) (gethash n *cache*)
               (if stored? 
                   val
                 (setf (gethash n *cache*) (cond ((= n 1) len)
                                                 ((evenp n) (+ len (helper (/ n 2) len)))
                                                 (t (+ len (helper (+ (* 3 n) 1) len)))))))))
    (helper n 1)))
