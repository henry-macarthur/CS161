(defun ANON(x)
    (cond ((not x) '())
    ((atom x) '0)
    (t (cons (ANON (car x)) (ANON (cdr x))))
    )
)