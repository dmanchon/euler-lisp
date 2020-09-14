;;this is my iterative version, based on bit array in order to improve speed too
(defun sieve (max)
  (let ((arr (make-array (1+ max) :element-type 'bit :initial-element 0)))
    (loop for i from 2 to max when (zerop (bit arr i)) do 
	 (loop for j from (expt i 2) to max by i do 
	      (setf (bit arr j) 1)) 
       and collect i )))

(defun euler-10 (&key (max 2000000))
  (reduce #'+ (sieve max)))

(compile 'sieve)
(compile 'euler-10)
;;(time (print (euler-10)))





;;algunas pruebas, intentos de hacer el algoritmo recursivo... (stackoverflow con numeros grandes)
(defun primep (n)                                                    
           (labels ((is-prime-aux (n div)                                       
                      (if (or (eq div n) (eq n 1) (> div (isqrt n)))                              
                          t                                                     
                          (if (eq (mod n div) 0)                                
                              nil                                    
                              (is-prime-aux n (1+ div))))))                     
             (is-prime-aux n 2)))

(defun euler-10b (n)
  (loop for i from 2 while (< i n) when (primep i) sum i))

(defun range (min max)
  (loop for i from min to max collect i))

(defun sieve-r (L)
  (labels ((sieve-helper (n L)
             (cond ((null L) (if (primep n) (list n) nil))
                   ((primep n) (append (list n) (sieve-helper (car L) (remove-if #'(lambda (x) (zerop (mod x n))) (cdr L)))))
                   (t (sieve-helper (car L) (cdr L))))))
    (sieve-helper (car L) (cdr L))))

;; de internet: http://rosettacode.org/wiki/Sieve_of_Eratosthenes
(defun sieve-of-eratosthenes (maximum)
  (let ((composites (make-array (1+ maximum) :element-type 'bit
                                             :initial-element 0)))
    (loop for candidate from 2 to maximum
          when (zerop (bit composites candidate))
            collect candidate
            and do (loop for composite from (expt candidate 2) to maximum by candidate
                         do (setf (bit composites composite) 1)))))

;;(time (reduce #'+ (remove-if-not #'primep (range 2 2000000))))
;;(time (reduce #'+ (sieve-of-eratosthenes 2000000)))

;;using normal array, performance is also quite good
(defun sieve-b (max)
  (let ((arr (make-array (1+ max) :initial-element 0)))
    (loop for i from 2 to max when (zerop (aref arr i)) do 
          (loop for j from (expt i 2) to max when (zerop (mod j i)) 
                do (setf (aref arr j) 1)) 
          and collect i )))
