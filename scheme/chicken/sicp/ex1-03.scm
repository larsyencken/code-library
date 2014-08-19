;
;  Define a procedure that takes three numbers as arguments and returns the
;  sum of the squares of the two larger numbers
;

(define (ss a b) (+ (* a a) (* b b)))

(define (s2 x y z)
  (if (> x y)
    (if (> y z)
      (ss x y)
      (ss x z))
    (if (> x z)
      (ss y x)
      (ss y z))))
