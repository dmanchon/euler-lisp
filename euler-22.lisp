(defun name-score (str)
  (reduce #'+ (mapcar #'(lambda (c) (- (char-code c)(char-code #\A) -1)) 
                      (loop for i across (string-upcase str) collect i))))

(defun split-by-char (string char)
  (loop for i = 0 then (1+ j)
     as j = (position char string :start i)
     collect (subseq string i j)
     while j))

(defun euler-22 (&key (file "names.txt"))
  (let* ((in (open file))
         (names (when in
                  (split-by-char (remove #\" (read-line in)) #\,)))
         (name-list (sort names #'string<)))
    (loop for i from 1 to (length name-list) sum (* i (name-score (nth (1- i) name-list)))))) 

(time (print (euler-22)))
