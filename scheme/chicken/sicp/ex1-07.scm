(define (sqrt-iter guess x)
  (let ((y (improve guess x)))
    (if (good-enough? guess y x)
      y
      (sqrt-iter y x)
    )))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess guess2 x)
  (< (/ (abs (- guess2 guess)) guess2) 0.0001))

(define (sqrt x) (sqrt-iter 1.0 x))

(define (square x) (* x x))

;  The good-enough? test used in computing square roots will not be very
;  effective for finding the square roots of very small numbers. Also, in real
;  computers, arithmetic operations are almost always performed with limited
;  precision. This makes our test inadequate for very large numbers. Explain
;  these statements, with examples showing how the test fails for small and
;  large numbers.
(sqrt 0.00000001)
; 0.0312501065624275

(sqrt 1e64)
; ...

;  An alternative strategy for implementing good-enough? is to watch how guess
;  changes from one iteration to the next and to stop when the change is
;  a very small fraction of the guess. Design a square-root procedure that
;  uses this kind of end test. Does this work better for small and large
;  numbers?
