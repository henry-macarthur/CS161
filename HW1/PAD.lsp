; PAD(n) = PAD(n−1)+PAD(n−2)+PAD(n−3)
; 1 1 1 3 5 9 
; 0 1 2 -> 1

(defun PAD(x)
    (if ( < x 3) 1
    (+ (PAD (- x 1)) (+ (PAD(- x 2)) (PAD(- x 3))))
    )
)

(print (PAD 14)) 

