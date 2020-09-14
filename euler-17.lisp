;; http://en.wikipedia.org/wiki/English_numerals

(defparameter *number-mapping*
  '((1 "one")
    (2 "two")
    (3 "three")
    (4 "four")
    (5 "five")
    (6 "six")
    (7 "seven")
    (8 "eight")
    (9 "nine")
    (10 "ten")
    (11 "eleven")
    (12 "twelve")
    (13 "thirteen")
    (14 "fourteen")
    (15 "fifteen")
    (16 "sixteen")
    (17 "seventeen")
    (18 "eighteen")
    (19 "nineteen")
    (20 "twenty")
    (30 "thirty")
    (40 "forty")
    (50 "fifty")
    (60 "sixty")
    (70 "seventy")
    (80 "eighty")
    (90 "ninety")
    (100 "hundred")
    (1000 "thousand")))

(defun range (min max)
  (loop for i from min to max collect i)) 

(defun digit-to-string (num)
  (second (assoc num *number-mapping* :test #'=)))

(defun num-to-string (number)
  (let* ((n (loop for digit in (reverse (map 'list #'digit-char-p (prin1-to-string number))) collect digit))
         (n0 (first n))
         (n1 (second n))
         (n2 (third n)))
    (concatenate 'string 
                 (when n2 (concatenate 'string (digit-to-string n2) "hundred" (if (not (= n1 n0 0)) "and")))
                 (when n1 (cond ((> n1 1) (concatenate 'string (digit-to-string (* 10 n1)) (digit-to-string n0)))
				((= n1 n0 0) "")
				((= n1 0) (concatenate 'string "and" (digit-to-string n0)))
				((zerop n1) (digit-to-string n0))
				(t (digit-to-string (+ n0 (* 10 n1))))))
		 (unless n1 (digit-to-string n0)))))

(defun euler-17 ()
  (+ (length "onethousand") (reduce #'+ (mapcar #'length (mapcar #'num-to-string (range 1 999))))))

(time (print (euler-17)))


;;but lisp already have the num-to-string function
;; (format nil "~R" 34) only we need to remove hyphens "-" and spaces ..... anyway is takes much more time and resources
;; tayloj posted this in euler forums, i found it very smart!!
(defun euler-17b ()
  (loop :for i :from 1 :to 1000
        :summing (count-if #'alpha-char-p (format nil "~R" i))))

;; investigation
(defun gen-numbers (min max)
  (loop for i from min to max collect
        (loop for digit in (reverse (map 'list #'digit-char-p (prin1-to-string i))) 
              collect digit)))

(defun num-to-string-old-1 (n)
  (let ((n0 (first n))(n1 (second n))(n2 (third n)))
    (concatenate 'string 
                 (if (not (null n2)) (concatenate 'string (digit-to-string n2) "hundred" (if (not (= n1 n0 0)) "and")))
                 (if (and (not (null n1)) (not (zerop n1))) 
                     (cond ((> n1 1) (concatenate 'string (digit-to-string (* 10 n1)) (digit-to-string n0)))
                           ((= n1 n0 0) "")
                           ((= n1 0) (concatenate 'string "and" (digit-to-string n0)))
                           (t (digit-to-string (+ n0 (* 10 n1)))))
                   (digit-to-string n0)))))

(defun num-to-string-old-2 (number)
  (let* ((n (loop for digit in (reverse (map 'list #'digit-char-p (prin1-to-string number))) collect digit))
         (n0 (first n))
         (n1 (second n))
         (n2 (third n)))
    (concatenate 'string 
                 (if (not (null n2)) (concatenate 'string (digit-to-string n2) "hundred" (if (not (= n1 n0 0)) "and")))
                 (if (and (not (null n1)) (not (zerop n1))) 
                     (cond ((> n1 1) (concatenate 'string (digit-to-string (* 10 n1)) (digit-to-string n0)))
                           ((= n1 n0 0) "")
                           ((= n1 0) (concatenate 'string "and" (digit-to-string n0)))
                           (t (digit-to-string (+ n0 (* 10 n1)))))
                   (digit-to-string n0)))))
