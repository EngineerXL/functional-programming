; 4.44

; Is character a delimiter?
(defun delimiterp (c)
        (member c '(#\Space #\Tab #\Newline)))

; Trims a word
(defun process-word (bag str)
        (if (null str) str (string-trim bag str)))

; Get substring with indexes [l, r) from str
; Returns empty string if l >= r
(defun substr (str l r)
        (if (>= l r) "" (subseq str l r)))

; Trims all words in a string
(defun process-string (bag str)
        ; Add one space to end of str to do all trim in one loop
        (let* ((s (concatenate 'string str " ")) (left -1) (n (length s)) (res ""))
        (loop for right from 0 to (- n 1)
                do (if (delimiterp (char s right)) (progn
                        (setq res (concatenate 'string res (process-word bag (substr s (+ left 1) right))))
                        (setq left right)
                        (setq res (concatenate 'string res  (coerce (list (char s right)) 'string))))))
        (subseq res 0 (- (length res) 1))))

(defun recursive-text-trim (bag lst res)
        (if (null lst) res
        (recursive-text-trim bag (rest lst) (cons (process-string bag (first lst)) res))))

(defun text-trim (bag lst)
        (let ((lst-copy lst) (res '()))
        (reverse (recursive-text-trim bag lst-copy res))))
