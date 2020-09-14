;; esta funcion la he copiado de internet
(defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))

(defparameter *triangle* 
  (let((in (open "triangle.txt")))
    (when in
      (loop for line = (read-line in nil)
	 while line collect (mapcar #'parse-integer (split-by-one-space line))))))

(defun get-xy (x y list)
  (nth x (nth y list)))

(defun sum-triangle (a b c)
  (max (+ a b) (+ a c)))

(defun calc-upper-row (list)
  (let ((y  (1- (length list))))
    (append (butlast (butlast list))
            (list (loop for x from 0 to (1- y) append
                        (list (sum-triangle (get-xy x (1- y) list) 
					    (get-xy x y list) 
					    (get-xy (1+ x) y list))))))))

(defun reduce-triangle (triangle)
  (labels ((helper (triangle len)
	     (cond ((= len 0) (caar triangle))
		   (t (helper (calc-upper-row triangle) (1- len))))))
    (helper triangle (1- (length triangle)))))

(defun euler-67 ()
  (reduce-triangle *triangle*))

(time (print (euler-67)))

;;usando split-sequence
;; (require 'asdf)
;; (require 'asdf-install)
;; (asdf:oos 'asdf:load-op :split-sequence)
;; (mapcar #'parse-integer (split-sequence:split-sequence #\Space "1 2 3 4 5"))