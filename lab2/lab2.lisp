; 2.35

(defun subsets (lst)
        (subsets_recursive lst `() `()))

(defun subsets_recursive (lst res tmp)
        (if (null lst)
                (append res (list tmp))
                (append
                (subsets_recursive (rest lst) res tmp)
                (subsets_recursive (rest lst) res (append tmp (list (car lst)))))))
