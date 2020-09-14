(defun fib (&key (limit 4000000))
  (labels (
           (fib-aux (x a b)
             (if (> a limit)
                 nil
               (cons (+ a b) (fib-aux (1+ x) b (+ a b))))))
    (fib-aux 0 0 1)))

(time (reduce #'+ (remove-if-not #'evenp (fib))))