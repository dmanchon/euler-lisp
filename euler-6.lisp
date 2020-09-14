(defun range (init final)
  (loop for i from init to final collect i))

(defun square (x)
  (* x x))

(defun euler-6 (limit)
  (- (square (reduce #'+ (range 1 limit))) (reduce #'+ (mapcar #'square (range 1 limit)))))



