(defun SUMS(x)
    (if ( < x 3) 0
        (+ 2 (+ (SUMS(- x 1)) (+ (SUMS(- x 2)) (SUMS(- x 3)))))

    )
)


(print (SUMS 1))
(print (SUMS 2))
(print (SUMS 3))
(print (SUMS 4))
(print (SUMS 5))
(print (SUMS 6))
(print (SUMS 7))
(print (SUMS 8))
(print (SUMS 9))
(print (SUMS 10))
