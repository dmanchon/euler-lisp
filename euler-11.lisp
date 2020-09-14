(defconstant euler11-matrix (make-array '(20 20) :initial-contents
'((08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08)
  (49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00)
  (81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65)
  (52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91)
  (22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80)
  (24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50)
  (32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70)
  (67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21)
  (24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72)
  (21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95)
  (78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92)
  (16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57)
  (86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58)
  (19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40)
  (04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66)
  (88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69)
  (04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36)
  (20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16)
  (20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54)
  (01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48))))


(defun get-array-column (arr init-y init-x len)
  (reduce #'* (loop for y from init-y to (+ init-y (1- len)) collect
		   (aref arr y init-x))))

(defun get-array-row (arr init-y init-x len)
  (reduce #'* (loop for x from init-x to (+ init-x (1- len)) collect
		   (aref arr init-y x))))

;; probably isnt the smartest way to calculate the diagonals...
(defun get-array-diag-up (arr init-y init-x len)
  (reduce #'* (loop for x from 0 to (1- len) collect
		   (aref arr (- (+ init-y (1- len)) x) (+ init-x x)))))

(defun get-array-diag-down (arr init-y init-x len)
  (reduce #'* (loop for x from 0 to (1- len) collect
		   (aref arr (+ init-y x) (+ init-x x)))))
  
(defun get-block (arr x y block-size)
  "Buscamos el maximo de todas las columnas, filas y las 2 diagonales"
  (reduce #'max (append (loop for i from y to (+ y (1- block-size)) collect (get-array-row arr i x block-size))
                        (loop for i from x to (+ x (1- block-size)) collect (get-array-column arr y i block-size))
                        (list (get-array-diag-down arr y x block-size))
                        (list (get-array-diag-up arr y x block-size)))))


(defun euler-11 (&key (arr euler11-matrix) (block-size 4))
  "Recorremos la matriz para encontrar la submatrix con mayor peso"
  (let ((size (array-dimension arr 1)))
    (reduce #'max (loop for j from 0 to (- size block-size) append 
                        (loop for i from 0 to (- size block-size) collect 
                              (get-block arr i j block-size))))))

(time (print (euler-11)))

;; for testing
(defconstant test-matrix (make-array '(20 20) :initial-contents
'((08 02 22 97 38 15 00 40 00 75 40 05 07 78 52 12 02 02 02 02)
  (49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 01 01 01 01)
  (81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 03 03 03 03)
  (52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 04 04 04 04)
  (22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80)
  (24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50)
  (32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70)
  (67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21)
  (24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72)
  (21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95)
  (78 17 53 28 22 75 31 67 15 99 91 01 04 62 16 14 09 53 56 92)
  (16 39 05 42 96 99 31 47 55 99 88 24 00 17 54 24 36 29 85 57)
  (86 56 00 48 99 71 89 07 05 99 44 37 44 60 21 58 51 54 17 58)
  (19 80 81 99 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40)
  (04 52 99 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66)
  (88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69)
  (01 01 01 01 38 25 39 11 24 94 72 18 08 46 29 32 99 98 99 99)
  (01 01 01 01 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16)
  (01 01 01 01 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54)
  (01 01 01 01 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48))))
