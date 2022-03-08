; 1.48

(defun odd (x) (logand x 1))

(defun mul2 (x) (ash x 1))

(defun div2 (x) (ash x -1))

(defun fast* (x y)
(cond
    ((= y 0) 0)
    ((= (odd y) 1) (+ x (fast* (mul2 x) (div2 y))))
    ((fast* (mul2 x) (div2 y)))))
