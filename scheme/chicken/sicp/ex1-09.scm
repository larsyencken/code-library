; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-11.html

(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))
; this method uses defered operations: it is a linear recursive process

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))
; this method uses tail recursion: it is a linear iterative process
