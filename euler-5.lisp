(defun euler-5 (limit)
  (apply #'lcm (loop for i from 1 to limit collect i)))