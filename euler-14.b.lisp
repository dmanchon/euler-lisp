(defvar *collatz-serie-m*)

(defun collatz (n)
	   (if (evenp n) (/ n 2) (+ (* 3 n) 1)))

;; this is actually very nice
(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
              val
            (setf (gethash args cache)
                  (apply fn args)))))))

(defun collatz-serie (n)
  (cond ((= n 1) (list 1))
	((evenp n) (cons n (funcall *collatz-serie-m* (/ n 2))))
	(t (cons n (funcall *collatz-serie-m* (+ (* 3 n) 1))))))

(setq *collatz-serie-m* (memoize #'collatz-serie))

(defun collatz-serie-len (n)
  (length (collatz-serie n)))

(defun gen-series-pairs (n)
  (loop for i from 1 to n collect 
       (list (collatz-serie-len i) i)))

(defun euler-14 (&key (n 1000000))
  (car (sort (gen-series-pairs n) #'(lambda (x y) (> (car x) (car y))))))

;;(time (print (euler-14)))