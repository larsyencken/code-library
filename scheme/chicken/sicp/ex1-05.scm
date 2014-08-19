;
;  Ben Bitdiddle has invented a test to determine whether the interpreter he
;  is faced with is using applicative-order evaluation or normal-order
;  evaluation. He defines the following two procedures:
;

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;  Then he evaluates the expression

(test 0 (p))

;  What behavior will Ben observe with an interpreter that uses applicative-order evaluation?

(if (= (0 0) 0 (p)))
0

;  What behavior will he observe with an interpreter that uses normal-order
;  evaluation?
(test 0 (p))
(test 0 (p))
(test 0 (p))
(test 0 (p))
; ...etc
