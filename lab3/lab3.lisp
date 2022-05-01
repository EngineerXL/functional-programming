; 3.47

; Returns indexes of element with maximal absolute value
(defun find-max-ind (a)
        (setq n (array-dimension a 0))
        (setq m (array-dimension a 1))
        (setq res_i 0)
        (setq res_j 0)
        (loop for i from 0 to (- n 1)
                do (loop for j from 0 to (- m 1)
                        do (if (> (abs (aref a i j)) (abs (aref a res_i res_j)))
                                (progn (setq res_i i) (setq res_j j)))))
        (values res_i res_j))

; Swaps rows i1 and i2
(defun swap-row (a i1 i2)
        (princ "Swapping row ")
        (princ (+ i1 1))
        (princ " and ")
        (princ (+ i2 1))
        (terpri)
        (setq m (array-dimension a 1))
        (loop for j from 0 to (- m 1)
                do (rotatef (aref a i2 j) (aref a i1 j))))

; Swaps columns j1 and j2
(defun swap-col (a j1 j2)
        (princ "Swapping column ")
        (princ (+ j1 1))
        (princ " and ")
        (princ (+ j2 1))
        (terpri)
        (setq n (array-dimension a 0))
        (loop for i from 0 to (- n 1)
                do (rotatef (aref a i j1) (aref a i j2))))

; Returns matrix copy
(defun copy-matrix (a)
        (setq n (array-dimension a 0))
        (setq m (array-dimension a 1))
        (setq res (make-array (list n m) :initial-element 0.0))
        (loop for i from 0 to (- n 1)
                do (loop for j from 0 to (- m 1)
                        do (setf (aref res i j) (aref a i j))))
        res)

(defun swap-max-to-top-left (matr)
        (setq a (copy-matrix matr))
        (setq i (nth-value 0 (find-max-ind a)))
        (setq j (nth-value 1 (find-max-ind a)))
        (if (> i 0) (swap-row a 0 i))
        (if (> j 0) (swap-col a 0 j))
        a)
