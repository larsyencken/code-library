;
;  Observe that our model of evaluation allows for combinations whose
;  operators are compound expressions. Use this observation to describe the
;  behavior of the following procedure:
;

(define (a-plus-abs-b a b)
    ((if (> b 0) + -) a b))

; add b if b is > 0, else subtract it
