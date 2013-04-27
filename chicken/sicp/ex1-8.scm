;
;  implement a cube root method
;

(define (cbrt x)
  (define (cbrt-iter old-guess x)
    (let ((new-guess (improve-guess old-guess x)))
      (if (good-enough? old-guess new-guess x)
        new-guess
        (cbrt-iter new-guess x))))

  (define (good-enough? old-guess new-guess x)
    (< (/ (abs (- x (* new-guess new-guess new-guess))) x) 0.0001))

  (define (improve-guess guess x)
    (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))
  (cbrt-iter 1.0 x))
