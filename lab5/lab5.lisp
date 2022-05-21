; 5.42

(defun make-term (&key order coeff)
        (list order coeff))

(defun order (term) (first term))

(defun coeff (term) (second term))

(defclass polynom ()
        ((var-symbol :initarg :var :reader var)
        (term-list :initarg :terms :reader terms)))

(defun odd (x) (logand x 1))

(defun div2 (x) (ash x -1))

(defun bpow (x y)
        (cond ((= y 0) 1.0)
        ((= (odd y) 1) (* x (bpow (* x x) (div2 y))))
        ((bpow (* x x) (div2 y)))))

(defun calc (order-coef-lst x0)
        (if (null order-coef-lst) 0.0
        (let* ((order-coef (first order-coef-lst))
        (order (first order-coef))
        (coef (second order-coef)))
        (+ (* coef (bpow x0 order)) (calc (rest order-coef-lst) x0)))))

(defmethod funcall-polynom ((p polynom) x0)
        (calc (terms p) x0))
