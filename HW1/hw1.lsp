(defun PAD(x) ;create the function
    (if ( < x 3) 1 ; if value is less than 3, base case, return 1
    (+ (PAD (- x 1)) (+ (PAD(- x 2)) (PAD(- x 3)))) ; else: return PAD(x-1) + PAD(x-2) + PAD(x-3)
    )
)

;/////

(defun SUMS(x) ;create the function
    (if ( < x 3) 0 ; if the value is less than 3, base, case, return 0
        (+ 2 (+ (SUMS(- x 1)) (+ (SUMS(- x 2)) (SUMS(- x 3))))) ; otherwise count pad sequence, and do +2 for each 
        ; calculation and a calculation has 2 plus signs
    )
)

;/////

(defun ANON(x) ; create the function
    (cond ((not x) '()) ; if input is null, return empty list
    ((atom x) '0) ; if input is a single item, return a 0 
    (t (cons (ANON (car x)) (ANON (cdr x)))) ; if input is a list,  return a list of first element combined with rest of the elements
    )
)