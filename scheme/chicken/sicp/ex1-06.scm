;  Alyssa P. Hacker doesn't see why if needs to be provided as a special form.
;  ``Why can't I just define it as an ordinary procedure in terms of cond?''
;  she asks. Alyssa's friend Eva Lu Ator claims this can indeed be done, and
;  she defines a new version of if:

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;  Eva demonstrates the program for Alyssa:
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

;  What happens when Alyssa attempts to use this to compute square roots?
;  Explain.

;  (Answer) Eager evaluation of arguments means the recursion never
;  terminates.
